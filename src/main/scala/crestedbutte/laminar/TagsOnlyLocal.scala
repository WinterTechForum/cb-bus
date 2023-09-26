package crestedbutte.laminar

import com.billding.time.{MinuteDuration, WallTime}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.*
import crestedbutte.dom.BulmaLocal
import crestedbutte.routes.{AllRoutes, RtaSouthbound, SpringFallLoop, TownShuttleTimes}
import org.scalajs.dom

import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.{Clock, OffsetDateTime}
import scala.concurrent.duration.FiniteDuration
import crestedbutte.dom.BulmaLocal.ModalMode
import org.scalajs.dom.{IDBDatabase, IDBEvent, IDBValue, IDBTransactionMode, window}

object TagsOnlyLocal {
  import com.raquo.laminar.api.L._

  def Plan(
    plan: Plan,
  ) =
    if (plan.legs.nonEmpty)
      div(
        "Plan: ",
        button(
          cls := "button",
          "Copy to Clipboard",
          onClick --> saveDailyPlan(plan),
          onClick --> Observer { _ =>
            dom.window.navigator.clipboard
              .writeText(plan.plainTextRepresentation)
          },
        ),
        plan.legs.zipWithIndex.map { case (routeLeg, idx) =>
          div(
            RouteLeg(
              "Trip " + (idx + 1),
              routeLeg,
            ),
          )
        },
      )
    else div()

  def RouteLeg(
    label: String,
    routeLeg: RouteLeg,
  ) =
    div(
      label,
      routeLeg.stops.map(stop =>
        createBusTimeElementOnLeg(
          stop.location,
          div(
            stop.busTime.toDumbAmericanString,
          ),
        ),
      ),
    )


  var tripDb: IDBDatabase = null

  import zio.json._

  private def createDb() = Observer {
    _ =>
//      println("Should try create DB")
      if (tripDb != null) {
//        println("DB already exists")
      } else {
        val dbRequest =
          window.indexedDB.get.open("CbBus", 2L)

        dbRequest.onsuccess = (db: IDBEvent[IDBDatabase]) =>
          println("Assigning DB")
          tripDb = db.target.result
          println("Assigned DB")

        dbRequest.onupgradeneeded = (db: IDBEvent[IDBDatabase]) =>
          println("creating object store")
          tripDb.createObjectStore("dailyPlans")
          println("creating object store")
      }
  }

  def saveDailyPlan(plan: Plan) = Observer {
    _ =>
      println("Saving daily plan")

      if (tripDb != null) {
        println("non-null DB. Let's try and save")
        val transaction = tripDb.transaction("dailyPlans", IDBTransactionMode.readwrite)
        val objectStore = transaction.objectStore("dailyPlans")
        val request = objectStore.put(plan.toJson, "today")
        request.onsuccess = (event: dom.Event) => {
          println("Successfully added plan to dailyPlans")
        }

      }
  }

  def retrieveDailyPlan($plan: Var[Plan]) = Observer {
    _ =>
      println("Retrieving daily plan")

      if (tripDb != null) {
        println("non-null DB. Let's try and save")
        val transaction = tripDb.transaction("dailyPlans", IDBTransactionMode.readwrite)
        val objectStore = transaction.objectStore("dailyPlans")
        val request = objectStore.get("today")
        request.onsuccess = (db: IDBEvent[IDBValue]) => {
          println("Retrieved item: " + db.target.result)
          val retrieved = db.target.result.toString.fromJson[Plan]
          $plan.set(retrieved.getOrElse(crestedbutte.Plan(Seq.empty)))
          println("Retrieved item: " + retrieved)
        }

      }
  }

  def RouteLegEnds(
    routeLeg: RouteLeg,
    $plan: Var[Plan],
  ) =
    div(
      div(
        button(
          cls := "button",
          "Add to Plan",
          onClick --> Observer { _ =>
            $plan.update(plan =>
              plan.copy(legs = plan.legs :+ routeLeg.ends),
            )
            println("New plan: " + $plan.now())
          },
        ),
      ),
      div:
        List(
          routeLeg.stops.head,
          routeLeg.stops.last,
        ).map: stop =>
          createBusTimeElementOnLeg(
            stop.location,
            div:
              stop.busTime.toDumbAmericanString,
          ),
    )

  def FullApp(
    pageMode: AppMode,
    initialRouteOpt: Option[String],
    javaClock: Clock,
  ) = {

    val clockTicks = new EventBus[Int]

    val components = AllRoutes.components(pageMode)

    val selectedRoute: Var[ComponentData] = Var(
      initialRouteOpt
        .flatMap(initialRoute =>
          components.find(
            _.componentName.elementNameMatches(initialRoute),
          ),
        )
        .getOrElse(
//          SpringFallLoop,
          TripPlannerComponent,
//          RtaSouthbound.fullSchedule,
        ),
    )

    def currentWallTime(
      javaClock: Clock,
    ) =
      WallTime(
        OffsetDateTime
          .now(javaClock)
          .toLocalTime
          .format(
            DateTimeFormatter.ofPattern("HH:mm"),
          ),
      )

    val initialTime = currentWallTime(javaClock)

    val timeStamps: Signal[WallTime] = clockTicks.events.foldLeft(
      initialTime,
    )(
      (
        _,
        _,
      ) => currentWallTime(javaClock),
    )

    div(
//      onLoad.mapTo(()) --> createDb(),
      Bulma.menu(selectedRoute, components),
      RepeatingElement()
        .repeatWithInterval( // This acts like a Dune thumper
          1,
          new FiniteDuration(2, scala.concurrent.duration.SECONDS),
        ) --> clockTicks,
      clockTicks --> createDb(),
      TagsOnlyLocal
        .overallPageLayout(
          selectedRoute.signal,
          timeStamps,
          pageMode,
          initialTime,
        ),
    )
  }

  def overallPageLayout(
    $selectedComponent: Signal[ComponentData],
    timeStamps: Signal[WallTime],
    pageMode: AppMode,
    initialTime: WallTime,
  ) = {
    // TODO Turn this into a Signal. The EventBus should be contained within the Experimental/FeatureControlCenter
    val featureUpdates = new EventBus[FeatureStatus]

    val initialFeatureSets = FeatureSets(
      Feature.values.map((_, false)).toMap,
    )

    val $enabledFeatures: Signal[FeatureSets] =
      featureUpdates.events
        .foldLeft[FeatureSets](initialFeatureSets) {
          case (currentFeatures, featureUpdate) =>
            currentFeatures.update(featureUpdate)
        }

    val gpsPosition: Var[Option[GpsCoordinates]] = Var(None)

    val planner = LaminarTripPlanner
      .TripPlannerLaminar(initialTime)

    val upcomingArrivalData =
      $selectedComponent.combineWith(timeStamps)
        .map { case (componentData, timestamp) =>
          // This is a super janky way to avoid being unable to scroll
          // after we refresh the page and close the model
          org.scalajs.dom.document
            .querySelector("html")
            .classList
            .remove("is-clipped")
          componentData match {
            case TripPlannerComponent => planner
            case namedRoute: NamedRoute =>
              TagsOnlyLocal.structuredSetOfUpcomingArrivals(
                TimeCalculations
                  .getUpComingArrivalsWithFullScheduleNonZio(
                    timestamp,
                    namedRoute,
                  ),
                $enabledFeatures,
                gpsPosition,
              )
          }
        }

    div(
      div(
        cls := "bill-box",
        idAttr := "container",
        child <-- upcomingArrivalData, // **THIS IS THE IMPORTANT STUFF** The fact that it's hard to see means I need to remove other bullshit
        timeStamps --> Observer[WallTime](
          onNext = localTime =>
            desiredAlarms
              .dequeueAll(busTime =>
                localTime
                  .between(busTime)
                  // TODO Direct comparison
                  .toMinutes <= NotificationStuff.headsUpAmount.toMinutes,
              )
              .map(
                Experimental.Notifications
                  .createJankyBusAlertInSideEffectyWay(_, localTime),
              ),
        ),
        Option.when(pageMode == AppMode.dev)(
          Experimental.Sandbox(
            timeStamps,
            gpsPosition,
            featureUpdates: EventBus[FeatureStatus],
          ),
        ),
      ),
    )
  }

  def renderWaitTime(
    duration: MinuteDuration,
  ) =
    if (duration.toMinutes == 0)
      "Leaving!"
    else
      duration.toMinutes + " min."

  def GeoBits(
    $mapLinksEnabled: Signal[Boolean],
    location: Location,
    $gpsPosition: Signal[Option[GpsCoordinates]],
  ) =
    div(
      child <-- $mapLinksEnabled.map(mapLinksEnabled =>
        if (mapLinksEnabled)
          div(
            cls := "map-link",
            child <-- Components
              .distanceFromCurrentLocationToStop($gpsPosition,
                                                 location,
              ),
            location.gpsCoordinates.map(Components.GeoLink),
          )
        else
          div(),
      ),
    )

  def createBusTimeElement(
    location: Location,
    content: ReactiveHtmlElement[_],
    $mapLinksEnabled: Signal[Boolean],
    $gpsPosition: Signal[
      Option[GpsCoordinates],
    ], // TODO Should this be an `Option[Signal[GpsCoordinates]` instead?
    /* TODO: waitDuration: Duration*/
  ) =
    div(
      width := "100%",
      cls := "stop-information",
      GeoBits($mapLinksEnabled, location, $gpsPosition),
      div(cls := "stop-name", div(location.name)),
      div(cls := "stop-alt-name", div(location.altName)),
      div(cls := "upcoming-information", content),
    )

  // TODO Dedup with above
  def createBusTimeElementOnLeg(
    location: Location,
    content: ReactiveHtmlElement[_],
  ) =
    div(
      width := "100%",
      cls := "stop-information",
      div(cls := "stop-name", div(location.name)),
      div(cls := "stop-alt-name", div(location.altName)),
      div(cls := "upcoming-information", content),
    )

  def StopTimeInfoForLocation(
    stopTimeInfo: StopTimeInfo,
    busScheduleAtStop: BusScheduleAtStop,
    $enabledFeatures: Signal[FeatureSets],
    namedRoute: NamedRoute,
  ) = {
    val modalActive = Var(false)
    val modalMode: Var[ModalMode] = Var(ModalMode.UpcomingStops)
    div(
      button(
        cls := "arrival-time button open-arrival-time-modal",
        onClick.preventDefault.map { _ =>
          org.scalajs.dom.document
            .querySelector("html")
            .classList
            .add("is-clipped")
          true
        } --> modalActive,
        stopTimeInfo.time.toDumbAmericanString,
      ),
      div(
        cls := "wait-time",
        renderWaitTime(stopTimeInfo.waitingDuration),
        BulmaLocal.bulmaModal(
          busScheduleAtStop,
          $enabledFeatures.map(
            _.isEnabled(Feature.BusAlarms),
          ),
          modalActive,
          modalMode,
          namedRoute,
        ),
      ),
    )
  }

  def RouteHeader(
    routeName: RouteName,
  ) =
    div(
      cls := "route-header",
      span(
        cls := "route-header_name",
        routeName.userFriendlyName + " Departures",
      ),
      Components.SvgIcon("glyphicons-basic-32-bus.svg"),
    )

  def structuredSetOfUpcomingArrivals(
    upcomingArrivalComponentData: UpcomingArrivalComponentData,
    $enabledFeatures: Signal[FeatureSets],
    gpsPosition: Var[Option[GpsCoordinates]],
  ) =
    div(
      RouteHeader(upcomingArrivalComponentData.routeName),
      upcomingArrivalComponentData.upcomingArrivalInfoForAllRoutes
        .map {
          case UpcomingArrivalInfoWithFullSchedule(
                UpcomingArrivalInfo(location, content),
                fullScheduleAtStop,
                namedRoute,
              ) =>
            TagsOnlyLocal.createBusTimeElement(
              location,
              content match {
                case Left(stopTimeInfo) =>
                  StopTimeInfoForLocation(
                    stopTimeInfo,
                    fullScheduleAtStop,
                    $enabledFeatures,
                    namedRoute,
                  )
                case Right(safeRideRecommendation) =>
                  Components.SafeRideLink(safeRideRecommendation)
              },
              $enabledFeatures.map(
//                _.isEnabled(Feature.MapLinks),
                _ => true, // Maps always enabled now
              ),
              gpsPosition.signal,
            )
        },
    )

}
