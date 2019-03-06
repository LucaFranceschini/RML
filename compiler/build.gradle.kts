plugins {
    java // Java support
    application // Executable JVM application support
    antlr // ANTLR parser generator
    kotlin("jvm").version("1.3.21") // JVM target for Kotlin compiler
}

repositories {
    mavenCentral()
}

dependencies {
    antlr("org.antlr:antlr4:4.+")
    implementation(kotlin("stdlib-jdk8")) // Kotlin standard library (use Java 8 features)
    compile("com.xenomachina:kotlin-argparser:2.0.7")
}

// Define the main class for the application
application { mainClassName = "rml.CompilerKt" }

// make System.in reads blocking when using Gradle
val run: JavaExec by tasks
run.standardInput = System.`in`

// prepare JAR artifact
val jar: Jar by tasks
// make it executable
jar.manifest { attributes["Main-Class"] = "rml.CompilerKt" }
// include dependencies (fat-JAR)
jar.from(configurations.runtime.get().map { if (it.isDirectory) it else zipTree(it) })

tasks {
    // first generate ANTLR parser, then compile source code
    compileKotlin { dependsOn(generateGrammarSource) }

    generateGrammarSource {
        // generate visitor rather than listener
        // must be specific due to overload resolution ambiguity
        arguments.plusAssign(listOf("-visitor", "-no-listener"))
    }
}