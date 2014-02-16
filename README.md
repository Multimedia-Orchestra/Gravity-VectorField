# What is this?

Hosts the source code for Gravity and Vector Field.

## How to run?

For Gravity, You need the LeapMotion runtime on your computer. This is included in the repo for Windows users (in the form of ```\*.dll```).

For VectorField, you need to install [SimpleOpenNi](https://code.google.com/p/simple-openni/).

### Gravity - Windows

1. Download the files in https://github.com/Multimedia-Orchestra/Gravity-VectorField/tree/master/out/artifacts/VectorField_jar
2. Run VectorField.bat (You can change the parameters in the .bat)

### Gravity - Mac (untested)

1. Download leap motion runtime
2. Download the .jar files in https://github.com/Multimedia-Orchestra/Gravity-VectorField/tree/master/out/artifacts/VectorField_jar
3. Run the VectorField.jar file, passing command line arguments as necessary from the command line

### Linux - Talk to me and we'll work it out!

### Command Line arguments
``` VectorField.jar <number of particles> <alpha value> <reset count>```

1. Number of particles - default 250,000; put more or less as your CPU can handle
2. alpha value (between 1 - 255) - default 14; how "white" each particle is; if you're having a hard time seeing particles, increase alpha
3. reset count - default 1200; reset the particles if there hasn't been interaction in this many frames

## How to build/change?

First get the prerequisites

1. [Scala](http://www.scala-lang.org/)
1. [Processing](http://processing.org/)
2. My personal library - [https://github.com/hellochar/Scala-libs](https://github.com/hellochar/Scala-libs)
3. [LeapMotion](https://github.com/voidplus/leap-motion-processing) for Gravity, [SimpleOpenNI](https://code.google.com/p/simple-openni/) for VectorField
4. I work in [IntelliJ IDEA](http://www.jetbrains.com/idea/features/scala.html); work in your own environment at your own risk! (but feel free to ask me if you have any questions; [hellocharlien@hotmail.com](mailto: hellocharlien@hotmail.com))

Then, set up your project:

1. download my source code ```git clone https://github.com/Multimedia-Orchestra/Gravity-VectorField```
2. Open the project with IntelliJ (Make sure you have libraries installed for Scala, Processing, Scala-misc, and LeapMotion), as well as a Scala facet
3. Set up the libraries:
    a. scala-library 2.10.3
    b. Processing jars (you need processing/core/library/core.jar in the classpath)
    c. Scala-misc (you need Misc.jar)
    d. LeapMotion (you need LeapMotionForProcessing/library/LeapJava.jar and LeapMotionForProcessing/library/LeapMotionForProcessing.jar)
    e. SimpleOpenNI (you need SimpleOpenNI/library/SimpleOpenNI.jar)
4. Run Gravity or VectorField

## Help!

Contact me at hellocharlien@hotmail.com for any questions!
