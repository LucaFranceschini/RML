# RML
## Step #1: Specification
The specification is written in RML and compiled to Prolog.

In order to build the compiler you need the [Gradle build tool](https://gradle.org/). The compiler is written using ANTLR, Java and Kotlin.

To build `cd` into `compiler` and type:

    $ ./gradlew build

To run the compiler execute the automatically generated JAR:

    $ java -jar build/libs/rml-compiler.jar

Run with `--help` for options.

## Step #2: Instrumentation
The program under test needs to run under an instrumentation layer observing and collecting all relevant events, generating an execution trace.

This is language-independent: the only requisite is to produce a text file with an event for each line, encoded in [JSON](https://www.json.org/).

### Node.js Instrumentation
We provide an instrumentation layer for Node.js based on [Jalangi2](https://github.com/Samsung/jalangi2/), a dynamic analysis framework based on code instrumentation.

In order to build this instrumentation you need Node.js installed as well as its package manager, [npm](https://www.npmjs.com/). To installed the required libraries `cd` into `instrumentation/nodejs` and type:

    $ npm install

To run a program with the instrumentation use the convenient Bash script:

    $ ./instrument.sh log-file.txt program.js [program-args ...]

The log file will contain the execution trace.

## Step #3: Verification
Our monitor is implemented in [SWI-Prolog](http://www.swi-prolog.org/), so you need to install it. Version 7+ needed.

To verify a trace `trace.txt` against a compiled specification `spec.pl`:

    $ monitor/monitor.sh spec.pl trace.txt

## Examples
See folder `examples` for RML specifications of Node.js properties, together with programs to monitor.
