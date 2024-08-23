package controllers

import models.{NewTodoListItem, TodoListItem}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import play.api.libs.json._

import javax.inject._
import scala.collection.mutable

/*
Marcamos la clase como una @Singleton para que el marco cree solo una instancia. Esto significa que lo reutilizará para cada solicitud.
 */

@Singleton
/*
Hemos usado la anotación @Inject para indicarle a Play Framework que pase automáticamente las dependencias de clase requeridas.
 */
class TodoListController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {

  private val todoList = new mutable.ListBuffer[TodoListItem]()
  todoList += TodoListItem(1, "test", isItDone = true)
  todoList += TodoListItem(2, "some other value", isItDone = false)
  todoList += TodoListItem(3, "Another Task", isItDone = false)

  implicit val todoListJson: OFormat[TodoListItem] = Json.format[TodoListItem]

  implicit val newTodoListJson = Json.format[NewTodoListItem]

  /*
  La accion nos da acceso a los parametros de la solicitudy nos deuelve una respuesta HTTP. en este cado solo nos retorna "NoContent "
   */
  def getAll: Action[AnyContent] = Action {
    if (todoList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(todoList))
    }
  }

  /*
  Nuestro método obtiene el parámetro itemId e intenta encontrar el elemento de la lista de tareas pendientes con el mismo id.
   */

  def getById(itemId: Long): Action[AnyContent] = Action {
    val foundItem = todoList.find(_.id == itemId)
    foundItem match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }

  def addNewItem() = Action { implicit request =>
    Ok("Got request" + request )
    val content = request.body
    val jsonObject = content.asJson
    val todoListItem: Option[NewTodoListItem] =
      jsonObject.flatMap(
        Json.fromJson[NewTodoListItem](_).asOpt
      )

    todoListItem match {
      case Some(newItem) =>
        val nextId = todoList.map(_.id).max + 1
        val toBeAdded = TodoListItem(nextId, newItem.description, isItDone = false)
        todoList += toBeAdded
        Created(Json.toJson(toBeAdded))
      case None =>
        BadRequest
    }


  }

  // curl -X DELETE localhost:9000/todo/done
  def deleteAllDone() = Action {
    todoList.filterInPlace(_.isItDone == false)
    Accepted
  }


  /*
   curl -X PUT localhost:9000/todo/done/1
   */
  def markAsDone(itemId: Long) = Action {
    val foundItem = todoList.find(_.id == itemId)
    foundItem match {
      case Some(item) =>
        val newItem = item.copy(isItDone = true)
        todoList.dropWhileInPlace(_.id == itemId)
        todoList += newItem
        Accepted(Json.toJson(newItem))
      case None => NotFound
    }
  }


}











