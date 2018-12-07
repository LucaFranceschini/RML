package rml.ast

/* The only purpose of this class is to factorize common constructor checks.
   Cannot factorize the property itself since data subclasses need to define at least a property.
   Declaring the property abstract does not work since the init block would find it uninitialized.
   So, every data subclass must declare the property and forward it to the constructor.
 */
abstract class Id(name: String) {
    // whatever the subclass, empty identifiers make no sense
    init {
        if (name.isEmpty())
            throw IllegalArgumentException("empty id not allowed")
    }
}

// variables from parametric and generic trace expressions
data class VarId(val name: String) : Id(name)

// variables representing trace expression terms
data class TraceExpId(val name: String) : Id(name)

// event type identifiers
data class EventTypeId(val name: String): Id(name)