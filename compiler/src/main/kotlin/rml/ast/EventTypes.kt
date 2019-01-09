package rml.ast

sealed class DataValue

sealed class SimpleValue: DataValue()
data class StringValue(val string: String): SimpleValue()
data class IntValue(val number: Int): SimpleValue()
data class VarValue(val id: VarId): SimpleValue() {
    constructor(id: String): this(VarId(id))
}
data class ListSimpleValue(val values: List<SimpleValue>): SimpleValue()

data class ObjectValue(val fields: List<Field>): DataValue() {
    data class Field(val key: KeyId, val value: DataValue) {
        constructor(key: String, value: DataValue): this(KeyId(key), value)
    }

    init {
        require(fields.isNotEmpty()) { "Non-empty object expected" }
    }
}

data class ListValue(val values: List<DataValue>): DataValue()

sealed class EvtypeDecl(open val evtype: EventType)
data class DirectEvtypeDecl(override val evtype: EventType, val patternValue: DataValue): EvtypeDecl(evtype) {
    init {
        require(checkOnlyOrAndObjects(patternValue)) {
            "(or-pattern of) objects expected at top-level event type declaration"
        }
    }

    private fun checkOnlyOrAndObjects(value: DataValue): Boolean = when (value) {
        is ObjectValue -> true
        is OrPatternValue -> checkOnlyOrAndObjects(value.left) && checkOnlyOrAndObjects(value.right)
        else -> false
    }
}
data class DerivedEvtypeDecl(override val evtype: EventType, val parents: List<EventType>): EvtypeDecl(evtype) {
    init {
        require(parents.isNotEmpty()) { "at least one parent expected" }
    }
}

data class EventType(val id: Id, val dataValues: List<SimpleValue>) {
    data class Id(val name: String): AbstractId(name)

    constructor(id: String, dataValues: List<SimpleValue>): this(Id(id), dataValues)
}