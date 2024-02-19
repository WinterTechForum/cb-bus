package crestedbutte.laminar

import com.billding.time.{TimePicker, WallTime}
import crestedbutte.*
import crestedbutte.TripBoundary
import crestedbutte.pwa.Persistence
import crestedbutte.routes.{RtaNorthbound, RtaSouthbound}
import org.scalajs.dom
import org.scalajs.dom.IDBDatabase
import website.webcomponents.material.{Button, SmartTimePicker}

object LaminarTripPlanner {
  import com.raquo.laminar.api.L._

  val componentName = ComponentName("TripPlanner")

  def demo() = {
    import org.scalajs.dom
    val app = div(
      LaminarTripPlanner.TripPlannerLaminar(WallTime("2:00 PM"),
                                            Persistence(),
      ),
    )
    render(
      dom.document.getElementById(componentName.name),
      app,
    )
  }

  def TripPlannerLaminar(
    initialTime: WallTime,
    db: Persistence,
  ) = {
    val routes =
      List(RtaNorthbound.fullSchedule, RtaSouthbound.fullSchedule)

    val $currentRoute: Var[NamedRoute] = Var(
      RtaSouthbound.fullSchedule,
    )
    val $tripBoundary: Var[TripBoundary] = Var(
      TripBoundary.StartingAfter,
    )

    val $startingPoint: Var[Location] = Var(
      $currentRoute.now().firstStopOnRoute,
    )

    val $destination: Var[Location] = Var(
      $currentRoute.now().lastStopOnRoute,
    )

    val TimePicker(timePicker, arrivalTimeS) =
      TimePicker(initialTime.toDumbAmericanString, 10)

    val changeBus = new EventBus[Unit]

    val submissionZ = new EventBus[TripParamZ]

    val tripResult: EventStream[Either[TripPlannerError, RouteLeg]] =
      submissionZ.events
        .map(_.evaluate())
    val $plan: Var[Option[Plan]] = Var(None)

    val valuesDuringChangeZ: EventStream[TripParamZ] =
      changeBus.events
        .withCurrentValueOf(
          $startingPoint,
          $destination,
          arrivalTimeS,
          $currentRoute,
          $tripBoundary,
        )
        .map:
          case (
                startingPoint,
                destination,
                arrivalTime,
                startRoute,
                tripBoundary,
              ) =>
            tripBoundary match
              case TripBoundary.StartingAfter =>
                TripParamZ.StartingAfter(
                  startingPoint,
                  arrivalTime,
                  startRoute.routeWithTimes,
                  destination,
                )
              case TripBoundary.ArrivingBy =>
                TripParamZ.ArrivingBy(
                  startingPoint,
                  arrivalTime,
                  destination,
                  startRoute.routeWithTimes,
                )

    div(
//      onMountCallback(_ => db.retrieveDailyPlan($plan)),
      valuesDuringChangeZ --> submissionZ,
      $startingPoint.signal
        .combineWith($destination,
                     $currentRoute,
                     $tripBoundary,
                     arrivalTimeS,
        )
        .changes
        .map(_ => ()) --> changeBus.writer,
      RouteSelector($currentRoute, $startingPoint, $destination),
      StopSelector("Starting from: ", $startingPoint, $currentRoute),
      // TODO Fix: this should have a later stop selected by default
      StopSelector("Reaching: ", $destination, $currentRoute),
      TripBoundarySelector($tripBoundary),
      timePicker,
      div(
        child <-- tripResult.map:
          case Left(value) =>
            div:
              value.msg
          case Right(value) =>
            RouteLegEnds(value, $plan),
      ),
      child <-- $plan.signal.map(plan =>
        div(
          plan match
            case Some(value) => SavePlanButton(value, db, $plan)
            case None => span()
          ,

          button(
            cls := "button",
            "Delete saved plan",
            onClick --> db.saveDailyPlan(
              crestedbutte.Plan(Seq.empty),
              $plan
            ),
          ),
          plan match
            case Some(value) => Components.PlanElement(value, db, $plan)
            case None => div()
          ,

        ),
      ),
      EventStream.unit() --> changeBus.writer,
    )
  }

  def TripBoundarySelector(
    $tripBoundary: Var[TripBoundary],
  ) =
    div(
      cls := "control",
      label(
        cls := "radio",
        input(
          typ := "radio",
          nameAttr := "tripBoundarySelection",
          defaultChecked := $tripBoundary.now() == TripBoundary.ArrivingBy,
          onClick.mapTo(TripBoundary.ArrivingBy) --> $tripBoundary,
        ),
        "Arriving By",
      ),
      label(
        cls := "radio",
        input(
          typ := "radio",
          nameAttr := "tripBoundarySelection",
          defaultChecked := $tripBoundary.now() == TripBoundary.StartingAfter,
          onClick.mapTo(TripBoundary.StartingAfter) --> $tripBoundary,
        ),
        "Leaving After",
      ),
    )

  private object StopSelector:
    case class SelectValue(
      uniqueValue: String,
      humanFriendlyName: String)

    implicit val location2selectorValue: Location => SelectValue =
      (location: Location) =>
        SelectValue(location.name, location.name)

    def apply(
      label: String,
      $selection: Var[Location],
      $currentRoute: Var[NamedRoute],
    ) = {

      // TODO Lot of ugly code to work through in this method
      def Selector[T](
        route: Seq[T],
        stopSelection: Var[T],
      )(implicit converterThatCouldBeATypeClass: T => SelectValue,
      ) = {

        val valueMap: Map[SelectValue, T] =
          route
            .map: selectValue =>
              (converterThatCouldBeATypeClass(selectValue),
               selectValue,
              )
            .toMap
        val selectValues = route.map(converterThatCouldBeATypeClass)
        span(
          cls := "select is-rounded",
          select(
            inContext { thisNode =>
              onChange
                .mapTo:
                  thisNode.ref.value
                .map: uniqueValue =>
                  selectValues
                    .find(_.uniqueValue == uniqueValue)
                    .get
                .map(
                  valueMap.getOrElse(_,
                                     throw new RuntimeException(
                                       "can't find the value!",
                                     ),
                  ),
                ) --> stopSelection.writer
            },
            selectValues.map(stop =>
              option(
                selected := valueMap(stop) == stopSelection.now(),
                value(stop.uniqueValue),
                stop.humanFriendlyName,
              ),
            ),
          ),
        )
      }

      div(
        label,
        child <--
          $currentRoute.signal
            .map:
              _.allStops
            .map: route =>
              Selector(
                route,
                $selection,
              ),
      )
    }

  private def RouteSelector(
    $currentRoute: Var[NamedRoute],
    $startingPoint: Var[Location],
    $destination: Var[Location],
  ) =
    val fullRouteAndStopsUpdater =
      Observer[NamedRoute](
        onNext = route => {
          $startingPoint.set($destination.now())
          $destination.set(route.allStops.last)
          $currentRoute.update(_ => route)
        },
      )
    div(
      cls := "control",
      label(
        cls := "radio",
        input(
          typ := "radio",
          nameAttr := "routeSelection",
          onClick.mapTo(
            RtaNorthbound.fullSchedule,
          ) --> fullRouteAndStopsUpdater,
        ),
        RtaNorthbound.fullSchedule.componentName.userFriendlyName,
      ),
      label(
        cls := "radio",
        input(
          typ := "radio",
          nameAttr := "routeSelection",
          defaultChecked := true,
          onClick.mapTo(
            RtaSouthbound.fullSchedule,
          ) --> fullRouteAndStopsUpdater,
        ),
        RtaSouthbound.fullSchedule.componentName.userFriendlyName,
      ),
    )

  private def SavePlanButton(
    plan: Plan,
    db: Persistence,
    $plan: Var[Option[Plan]]
  ) =
    button(
      cls := "button",
      "Save Plan",
      onClick --> db.saveDailyPlan(plan, $plan),
    )

  private def RouteLegEnds(
    routeLeg: RouteLeg,
    $plan: Var[Option[Plan]],
  ) =
    div(
      div(
        button(
          cls := "button",
          "Add to Plan",
          onClick --> Observer { _ =>
            $plan.update {
              case Some(value) => Some(value.copy(legs = value.legs :+ routeLeg.ends))
              case None => None
            }
            println("New plan: " + $plan.now())
          },
        ),
      ),
      div:
        List(
          routeLeg.stops.head,
          routeLeg.stops.last,
        ).map: stop =>
          Components.UpcomingStopInfo(
            stop.location,
            div:
              stop.busTime.toDumbAmericanString,
          ),
    )

}
