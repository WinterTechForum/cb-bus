package crestedbutte

import org.scalajs.dom
import scala.scalajs.js

/** Detects browser capabilities for features that have inconsistent
  * cross-browser support.
  *
  * Key limitation: Safari iOS does NOT support the notification `tag`
  * option for replacing/updating existing notifications. Each call to
  * showNotification() creates a new notification instead of updating
  * the existing one. This makes countdown-style notifications
  * unreliable on Safari iOS.
  *
  * See: https://github.com/mdn/browser-compat-data/issues/19318
  */
object BrowserCapabilities {

  /** Detects if the browser is Safari on iOS. Safari iOS has broken
    * notification tag support - it creates new notifications instead
    * of replacing existing ones with the same tag.
    */
  def isSafariIOS: Boolean = {
    val userAgent = dom.window.navigator.userAgent
    val isIOS = userAgent.contains("iPhone") ||
      userAgent.contains("iPad") ||
      userAgent.contains("iPod")
    val isSafari = userAgent.contains("Safari") &&
      !userAgent.contains("Chrome") &&
      !userAgent.contains("CriOS") &&
      !userAgent.contains("FxiOS")
    isIOS && isSafari
  }

  /** Detects if the browser is Firefox. Firefox has limited support
    * for the `renotify` option but generally supports tag-based
    * notification replacement.
    */
  def isFirefox: Boolean =
    dom.window.navigator.userAgent.contains("Firefox")

  /** Detects if the browser supports the Notification API at all. */
  def hasNotificationAPI: Boolean =
    !js.isUndefined(dom.window.asInstanceOf[js.Dynamic].Notification)

  /** Detects if the browser has a service worker. */
  def hasServiceWorker: Boolean = {
    val navigatorDyn = dom.window.navigator.asInstanceOf[js.Dynamic]
    !js.isUndefined(navigatorDyn.serviceWorker) &&
    navigatorDyn.serviceWorker != null
  }

  /** Returns true if we can reliably show and UPDATE notifications
    * using the tag option. This is false on Safari iOS where the tag
    * option is completely ignored.
    *
    * When this returns false, we should NOT show the notification
    * bell button at all, because we cannot provide a reliable
    * countdown experience.
    */
  def supportsNotificationTagReplacement: Boolean =
    hasNotificationAPI && hasServiceWorker && !isSafariIOS

  /** Returns a human-readable explanation of why notifications are
    * not supported, if applicable.
    */
  def notificationUnsupportedReason: Option[String] =
    if (!hasNotificationAPI)
      Some("Your browser does not support notifications.")
    else if (!hasServiceWorker)
      Some("Your browser does not support service workers.")
    else if (isSafariIOS)
      Some(
        "Safari on iOS does not support updating notifications. " +
          "The countdown feature is not available on this browser.",
      )
    else
      None
}
