package crestedbutte.laminar

import com.raquo.airstream.ownership.DynamicSubscription
import com.raquo.airstream.ownership.Subscription
import com.raquo.laminar.modifiers.Binder
import com.raquo.laminar.nodes.ReactiveElement

trait BinderWithStartStop[-El <: ReactiveElement.Base]
    extends Binder[El] {

  protected var subscribed = false

  def doStart(): Unit
  def doStop(): Unit

  def stop(): Unit =
    if (subscribed) {
      doStop()
    }

  def start(): Unit =
    if (subscribed) {
      doStart()
    }

  override def bind(
    element: El,
  ): DynamicSubscription =
    ReactiveElement.bindSubscriptionUnsafe(element) { ctx =>
      subscribed = true
      start()

      new Subscription(ctx.owner,
                       cleanup = () => {
                         stop()
                         subscribed = false
                       },
      )
    }

}
