package rml.ast

sealed class DataValue
sealed class SimpleValue: DataValue()
data class StringValue(val string: String): SimpleValue()
data class IntValue(val number: Int): SimpleValue()
data class VarValue(val id: VarId): SimpleValue() {
    constructor(id: String): this(VarId(id))
}
data class ObjectValue(val fields: List<Field>): DataValue() {
    data class Field(val key: KeyId, val value: DataValue) {
        constructor(key: String, value: DataValue): this(KeyId(key), value)
    }

    init {
        require(fields.isNotEmpty()) { "Non-empty object expected" }
    }
}

sealed class EvtypeDecl(open val evtype: EventTypeTraceExp)
data class DirectEvtypeDecl(override val evtype: EventTypeTraceExp, val objects: List<ObjectValue>): EvtypeDecl(evtype) {
    init {
        require(objects.isNotEmpty()) { "at least one object pattern expected" }
    }
}
data class DerivedEvtypeDecl(override val evtype: EventTypeTraceExp, val parents: List<EventTypeTraceExp>): EvtypeDecl(evtype) {
    init {
        require(parents.isNotEmpty()) { "at least one parent expected" }
    }
}