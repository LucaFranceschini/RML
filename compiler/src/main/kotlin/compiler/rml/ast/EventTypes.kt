package compiler.rml.ast

sealed class EventTypeDeclaration

data class DirectEventTypeDeclaration(val eventType: EventType,
                                      val eventExpression: EventExpression,
                                      val negated: Boolean,
                                      val withDataExpression: DataExpression?): EventTypeDeclaration()

data class DerivedEventTypeDeclaration(val eventType: EventType,
                                       val parents: List<EventType>,
                                       val negated: Boolean,
                                       val withDataExpression: DataExpression?): EventTypeDeclaration() {
    init {
        require(parents.isNotEmpty()) { "at least one parent expected" }
    }
}

data class EventType(val name: Identifier, val parameters: List<Parameter>) {
    // for some reason it only compiles if I put subclasses inside the sealed class...
    sealed class Parameter {
        data class Expression(val eventExpression: EventExpression): Parameter()
        data class Variable(val variable: Identifier): Parameter() {
            // allow construction from String directly
            constructor(variable: String): this(Identifier(variable))
        }
    }

    // allow construction from String directly
    constructor(name: String, parameters: List<Parameter>): this(Identifier(name), parameters)
}