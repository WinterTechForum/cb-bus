package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** A simple bottom sheet component for mobile-friendly menus.
  * 
  * When open, displays a sheet sliding up from the bottom with
  * a semi-transparent backdrop. Clicking the backdrop closes it.
  */
object BottomSheet {
  
  case class MenuItem(
    icon: String,
    label: String,
    onClick: () => Unit,
    disabled: Boolean = false,
    hidden: Boolean = false,
  )
  
  def apply(
    isOpen: Var[Boolean],
    title: String,
    items: Signal[Seq[MenuItem]],
  ) = {
    div(
      cls := "bottom-sheet-container",
      cls <-- isOpen.signal.map(open => if (open) "bottom-sheet-open" else ""),
      
      // Backdrop
      div(
        cls := "bottom-sheet-backdrop",
        onClick --> Observer { _ => isOpen.set(false) },
      ),
      
      // Sheet content
      div(
        cls := "bottom-sheet",
        
        // Handle bar for visual affordance
        div(cls := "bottom-sheet-handle"),
        
        // Title
        div(
          cls := "bottom-sheet-title",
          title,
        ),
        
        // Menu items
        div(
          cls := "bottom-sheet-items",
          children <-- items.map { menuItems =>
            menuItems.filterNot(_.hidden).map { item =>
              button(
                cls := "bottom-sheet-item",
                cls := (if (item.disabled) "bottom-sheet-item-disabled" else ""),
                disabled := item.disabled,
                span(cls := "bottom-sheet-item-icon", item.icon),
                span(cls := "bottom-sheet-item-label", item.label),
                onClick --> Observer { _ =>
                  if (!item.disabled) {
                    item.onClick()
                    isOpen.set(false)
                  }
                },
              )
            }
          },
        ),
      ),
    )
  }
}
