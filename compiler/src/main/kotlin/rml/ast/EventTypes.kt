package rml.ast

sealed class EvtypeDecl(open val evtype: EventType, open val negated: Boolean = false)
data class DirectEvtypeDecl(override val evtype: EventType,
                            val patternValue: DataValue,
                            override val negated: Boolean = false): EvtypeDecl(evtype, negated) {
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
data class DerivedEvtypeDecl(override val evtype: EventType,
                             val parents: List<EventType>,
                             override val negated: Boolean = false): EvtypeDecl(evtype, negated) {
    init {
        require(parents.isNotEmpty()) { "at least one parent expected" }
    }
}

data class EventType(val id: Id, val dataValues: List<SimpleValue>) {
    data class Id(val name: String): AbstractId(name)

    constructor(id: String, dataValues: List<SimpleValue>): this(Id(id), dataValues)
}