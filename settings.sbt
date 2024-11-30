Global / excludeLintKeys ++= Set(idePackagePrefix, ideExcludedDirectories)

Global / ideExcludedDirectories := Seq(file(".bsp"), file(".idea"), file("target"))

initialize := {
  // Ensure previous initializations are run
  val _ = initialize.value
  val classVersion = sys.props("java.class.version")
  val specVersion = sys.props("java.specification.version")
  assert(specVersion.toDouble >= 21, "Java 21 or above is required to run this project.")
}
