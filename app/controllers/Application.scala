package controllers

import javax.inject.Inject
import play.api.data.Form
import play.api.data.Forms.nonEmptyText
import play.api.mvc.Action
import play.api.mvc.Controller
import services.CoinChange

class Application @Inject() (coinChange: CoinChange) extends Controller {

  val coinForm: Form[String] = Form(("coins", nonEmptyText.verifying(coinChange.valid _)))

  def index() = Action { request =>
    Ok(views.html.index(coinForm, Nil, None))
  }

  def calcChange() = Action(request => {
    coinForm.bindFromRequest()(request).fold(
      hasErrors => BadRequest(views.html.index(hasErrors, Nil, Some("Don't understand this, try £pounds.pence"))),
      success => {
        val result = coinChange.calculateChange(success)
        Ok(views.html.index(coinForm.fill(success), result.toList.sortBy(_._1.amount), None))
      })
  })
}
