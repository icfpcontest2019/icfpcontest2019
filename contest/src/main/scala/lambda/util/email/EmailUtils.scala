package lambda.util.email

/**
  * @author Marius Soutier, Ilya Sergey
  *         Adapted from https://gist.github.com/mariussoutier/3436111
  */

object EmailUtils {

  val SMTP_PORT = 25
  val SMTP_SERVER = "smtp.cs.ucl.ac.uk"

  val FROM_ADDRESS = "scenario@cs.ucl.ac.uk"
  val FROM_NAME = "UCL-CS Scenario Week Mailer"

  implicit def stringToSeq(single: String): Seq[String] = Seq(single)
  implicit def liftToOption[T](t: T): Option[T] = Some(t)

  sealed abstract class MailType
  case object Plain extends MailType
  case object Rich extends MailType
  case object MultiPart extends MailType

  case class Mail(to: Seq[String],
                  cc: Seq[String] = Seq.empty,
                  bcc: Seq[String] = Seq.empty,
                  subject: String,
                  message: String,
                  richMessage: Option[String] = None,
                  attachment: Option[(java.io.File)] = None)

  def send(mail: Mail) {
    import org.apache.commons.mail._

    val format =
      if (mail.attachment.isDefined) MultiPart
      else if (mail.richMessage.isDefined) Rich
      else Plain

    val commonsMail: Email = format match {
      case Plain => new SimpleEmail().setMsg(mail.message)
      case Rich => new HtmlEmail().setHtmlMsg(mail.richMessage.get).setTextMsg(mail.message)
      case MultiPart =>
        val attachment = new EmailAttachment()
        attachment.setPath(mail.attachment.get.getAbsolutePath)
        attachment.setDisposition(EmailAttachment.ATTACHMENT)
        attachment.setName(mail.attachment.get.getName)
        new MultiPartEmail().attach(attachment).setMsg(mail.message)
    }

    mail.to foreach commonsMail.addTo
    mail.cc foreach commonsMail.addCc
    mail.bcc foreach commonsMail.addBcc

    commonsMail.setSmtpPort(SMTP_PORT)
    commonsMail.setHostName(SMTP_SERVER)

    commonsMail.setFrom(FROM_ADDRESS, FROM_NAME).setSubject(mail.subject).send()

  }
}