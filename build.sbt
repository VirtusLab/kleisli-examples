import Settings._

val kleisli: Project = (project in file(".")).
                       settings(commonSettings: _*).
                       settings(
                           name := "kleisli"
                         )



    