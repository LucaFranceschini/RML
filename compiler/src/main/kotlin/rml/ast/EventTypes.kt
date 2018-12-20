package rml.ast

sealed class EvtypeValue
data class StringValue(val string: String): EvtypeValue()
data class IntValue(val number: Int): EvtypeValue()
data class VarValue(val id: VarId): EvtypeValue() {
    constructor(id: String): this(VarId(id))
}
data class ObjectValue(val fields: List<Field>): EvtypeValue() {
    data class Field(val key: KeyId, val value: EvtypeValue) {
        constructor(key: String, value: EvtypeValue): this(KeyId(key), value)
    }

    init {
        require(fields.isNotEmpty()) { "Non-empty object expected" }
    }
}

sealed class EvtypeDecl
data class DirectEvtypeDecl(val evtype: EventTypeTraceExp, val objects: List<ObjectValue>): EvtypeDecl() {
    init {
        require(objects.isNotEmpty()) { "at least one object pattern expected" }
    }
}
data class DerivedEvtypeDecl(val evtype: EventTypeTraceExp, val parents: List<EventTypeTraceExp>): EvtypeDecl() {
    init {
        require(parents.isNotEmpty()) { "at least one parent expected" }
    }
}