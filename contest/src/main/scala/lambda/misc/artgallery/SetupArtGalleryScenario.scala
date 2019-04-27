package lambda.misc.artgallery

import lambda.util.project.{ScenarioInstance, SetupScenario}

/**
  * @author Ilya Sergey
  */

object SetupArtGalleryScenario extends SetupScenario {
  override val instance: ScenarioInstance = ArtGalleryInstance
}
