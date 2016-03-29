import scala.annotation.tailrec


object Widget {

  case class Widget(name: String, dependencies: List[String])

  val getWidget = List(
    Widget("1", List("2", "5")),
    Widget("2", List("3", "4")),
    Widget("3", List()),
    Widget("4", List()),
    Widget("5", List())).map { w => w.name -> w }.toMap

  def getWidgetTree(key: String): List[Widget] = {
    def addIfNotAlreadyContained(widgetList: List[Widget], widgetNameToAdd: String): List[Widget] = {
      if (widgetList.exists(_.name == widgetNameToAdd)) widgetList
      else widgetList :+ getWidget(widgetNameToAdd)
    }
    @tailrec
    def traverseTree(currentWidgets: List[Widget], acc: List[Widget]): List[Widget] = currentWidgets match {
      case Nil =>
        // If there are no more widgets in this branch return what we've traversed so far
        acc

      case Widget(name, Nil) :: rest =>
        // If the first widget is a leaf traverse the rest and add the leaf to the list of traversed
        traverseTree(rest, addIfNotAlreadyContained(acc, name))

      case Widget(name, dependencies) :: rest =>
        // If the first widget is a parent, traverse it's children and the rest and add it to the list of traversed
        traverseTree(dependencies.map(getWidget) ++ rest, addIfNotAlreadyContained(acc, name))

    }
    val root = getWidget(key)
    traverseTree(root.dependencies.map(getWidget) :+ root, List[Widget]())
  }
}