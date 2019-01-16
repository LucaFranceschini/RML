# RML
![overview](overview.pdf)

RML (Runtime Monitoring Language) is a system-agnostic formal specification language designed for runtime verification.
In order to be completely independent from the system under monitoring, the instrumentation layer and the monitor are kept strictly separate.
The instrumentation observes relevant events, encodes them in JSON and send them to the monitor for verification.

# Usage

## Step #1: Specification
The specification is written in RML and compiled to Prolog.

In order to build the compiler you need the [Gradle build tool](https://gradle.org/).
The compiler is written in ANTLR, Java and Kotlin.

To build the compiler, `cd` into `compiler` and type:

    $ ./gradlew build

An executable JAR will be produced in `build/libs`.
To run the compiler:

    $ java -jar build/libs/rml-compiler.jar

By default it will read a specification from standard input and print Prolog code to standard output.
Run with `--help` for options.

## Step #2: Instrumentation
The program under test needs to run under an instrumentation layer observing and collecting all relevant events, generating an execution trace.

This is language-independent: the only requisite is to produce a text file with an event for each line, encoded in [JSON](https://www.json.org/).

### Node.js Instrumentation
We provide an instrumentation layer for Node.js based on [Jalangi2](https://github.com/Samsung/jalangi2/), a dynamic analysis framework based on code instrumentation.

In order to build this instrumentation you need Node.js installed as well as its package manager, [npm](https://www.npmjs.com/). To installed the required libraries `cd` into `instrumentation/nodejs` and type:

    $ npm install

To run a program with the instrumentation use the convenient Bash script:

    $ ./instrument.sh trace.txt program.js [program-args ...]

The text file will contain the execution trace.

## Step #3: Verification
Our monitor is implemented in [SWI-Prolog](http://www.swi-prolog.org/), so you need to install it. Version 7+ needed.

To verify a trace `trace.txt` against a compiled specification `spec.pl`:

    $ monitor/monitor.sh spec.pl trace.txt

# Examples
See folder `examples` for RML specifications of Node.js properties, together with programs to monitor.
