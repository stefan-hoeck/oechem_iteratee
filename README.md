An exploration of Iteratee-based bulk operations in cheminformatics using
OpenEye's OEChem toolkit

First, create a folder 'lib' in the project directory. Put oe java libraries
(oejava...-.jar and openeye-javautils.jar) as well as oechem binaries (.so-files)
directly into that folder. Note that the .so-files will not be on the classpath
if they are wrapped in the oejava.jar file.

Download and install Simple Build Tool (SBT) version 0.12.2 to compile and build.
Go to file Main.scala (found under src/main/scala/zhaw/oe) and uncomment
the one 'runl' method you would like to try out. See the comments there fore
command line arguments expected by the method.

The sbt-command 'one-jar' will compile and then build a single executable jar found at
target/scala-2.10/oe_2.10-1.0.0-SNAPSHOT-one-jar.jar
