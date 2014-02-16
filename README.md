# What is this?

Hosts the source code for Gravity and Vector Field.

## How to run?

You need the LeapMotion runtime on your computer. This is included in the repo for Windows users (in the form of ```\*.dll``` and ```LeapJava.jar```).

### Windows

1. Download the files in https://github.com/Multimedia-Orchestra/Gravity-VectorField/tree/master/out/artifacts/VectorField_jar
2. Run VectorField.bat (You can change the parameters in the .bat)

### Mac (untested)

1. Download leap motion runtime
2. Download the .jar files in https://github.com/Multimedia-Orchestra/Gravity-VectorField/tree/master/out/artifacts/VectorField_jar
3. Run the VectorField.jar file, passing command line arguments as necessary from the command line

### Command Line arguments
``` VectorField.jar <number of particles> <alpha value> <reset count>```

1. Number of particles - default 250,000; put more or less as your CPU can handle
2. alpha value (between 1 - 255) - default 14; how "white" each particle is; if you're having a hard time seeing particles, increase alpha
3. reset count - default 1200; reset the particles if there hasn't been interaction in this many frames

## How to build/change?

First get the prerequisites

1. Scala
1. Processing library
2. My personal library - https://github.com/hellochar/Scala-libs
3. Leap Motion SDK
4. I work in IntelliJ IDEA; work in your own environment at your own risk! (but feel free to ask me if you have any questions; [hellocharlien@hotmail.com](mailto: hellocharlien@hotmail.com))

Then, set up your project:

1. download my source code ```git clone https://github.com/Multimedia-Orchestra/Gravity-VectorField```
2. 
