package compiler.rml.ast

import compiler.prolog.ast.*

val directives: List<Directive> = listOf(
        // :- module('spec', [trace_expression/2, match/2]).
        Directive(CompoundTerm("module", atom("spec"), CompoundTerm.list(
                CompoundTerm("/", atom("trace_expression"), IntTerm(2)),
                CompoundTerm("/", atom("match"), IntTerm(2))
        ))),

        // :- use_module(monitor('deep_subdict')).
        Directive(CompoundTerm("use_module", CompoundTerm("monitor", atom("deep_subdict"))))
)

// event type declarations are directly compiled to Prolog
// the 2nd argument is the free variable identifier, it must not appear in the declaration
fun compile(declarations: List<EventTypeDeclaration>, eventName: String = "_event") =
        declarations.flatMap { compile(it, eventName) }

// event type compiler (inside specification expressions)
fun compile(eventType: EventType): CompoundTerm {
    // convert all parameters, whether they are expressions or variables
    val parameters = eventType.parameters.map { when (it) {
        // event expressions are not supported here
        is EventType.Parameter.EventExpression -> throw Exception("unexpected event expression")

        // variables should be wrapped as var(x) inside specification expressions, as here
        is EventType.Parameter.Variable -> CompoundTerm("var", atom(it.variable.name))

        is EventType.Parameter.DataExpression -> compile(it.dataExpression)
    } }

    return CompoundTerm(eventType.identifier.name, parameters)
}

// directly compile data expressions to Prolog (variables here should be wrapped as var(x))
fun compile(dataExpression: DataExpression): Term = when (dataExpression) {
    is BoolDataExpression -> atom(dataExpression.boolean.toString())
    is IntDataExpression -> IntTerm(dataExpression.int)
    is FloatDataExpression -> FloatTerm(dataExpression.double)
    is VariableDataExpression -> CompoundTerm("var", atom(dataExpression.variable.name))
    is SumDataExpression -> CompoundTerm("+", compile(dataExpression.left), compile(dataExpression.right))
    is SubDataExpression -> CompoundTerm("-", compile(dataExpression.left), compile(dataExpression.right))
    is LessThanDataExpression -> CompoundTerm("<", compile(dataExpression.left), compile(dataExpression.right))
    is LessThanEqualDataExpression -> CompoundTerm("=<", compile(dataExpression.left), compile(dataExpression.right))
    is GreaterThanDataExpression -> CompoundTerm(">", compile(dataExpression.left), compile(dataExpression.right))
    is GreaterThanEqualDataExpression -> CompoundTerm(">=", compile(dataExpression.left), compile(dataExpression.right))
    is EqualToDataExpression -> CompoundTerm("=:=", compile(dataExpression.left), compile(dataExpression.right))
    is AndDataExpression -> CompoundTerm(",", compile(dataExpression.left), compile(dataExpression.right))
    is OrDataExpression -> CompoundTerm(";", compile(dataExpression.left), compile(dataExpression.right))
}

private fun compile(declaration: EventTypeDeclaration, eventName: String = "_event"): List<Clause> {
    // only variables should appear as event type parameters
    if (declaration.eventType.parameters.any { it !is EventType.Parameter.Variable })
        throw Exception("unexpected expression in event type parameters")

    val parameters = declaration.eventType.parameters.filterIsInstance<EventType.Parameter.Variable>()

    // check the variable identifier is not used in arguments
    if (parameters.map { it.variable.name }.contains(eventName))
        throw Exception("the given identifier for the event variable is already in use")

    // match(_event, eventType(X1, ..., XN))
    val eventVariable = VariableTerm(eventName)
    val eventTypeTerm = CompoundTerm(
            declaration.eventType.identifier.name,
            parameters.map { VariableTerm(it.variable.name) })
    val head = CompoundTerm("match", eventVariable, eventTypeTerm)

    // generate a predicate for each parent event type or object pattern
    val parentPredicates = when (declaration) {
        // generate a deep_subdict (matching) predicate for every pattern instantiation
        is DirectEventTypeDeclaration -> compile(declaration.eventExpression).map {
            CompoundTerm("deep_subdict", it, eventVariable)
        }.toList()

        // generate a match predicate for every parent event type
        is DerivedEventTypeDeclaration -> declaration.parents.map { parentEventType ->
            val parentTerm = CompoundTerm(parentEventType.identifier.name, parameters.map {
                VariableTerm(it.variable.name)
            })
            CompoundTerm("match", eventVariable, parentTerm)
        }
    }

    // if not negated, just generate a clause for every predicate
    if (!declaration.negated)
        return parentPredicates.map { Clause(head, it) }

    // otherwise just generate one clause with all the negated predicates in the body
    return listOf(Clause(head, parentPredicates.map { CompoundTerm("not", it) }))
}

private fun compile(eventExpression: EventExpression): Sequence<Term> = sequence {
    when (eventExpression) {
        IgnoredEventExpression -> yield(VariableTerm("_"))
        is PatternEventExpression -> {
            yieldAll(compile(eventExpression.left))
            yieldAll(compile(eventExpression.right))
        }
        is ListEventExpression -> {
            val list = eventExpression.list
            if (list.isEmpty()) yield(EmptyList)
            else {
                val head = list.first()
                val tail = list.drop(1)

                // combine each term from the compilation of the head with each term from the compilation of the tail
                compile(head).forEach { headTerm ->
                    compile(ListEventExpression(tail)).forEach { tailTerm ->
                        yield(CompoundTerm("[|]", headTerm, tailTerm))
                    }
                }
            }
        }
        is StringEventExpression -> yield(atom(eventExpression.string))
        is IntEventExpression -> yield(IntTerm(eventExpression.number))
        is FloatEventExpression -> yield(FloatTerm(eventExpression.number))
        is BoolEventExpression -> yield(atom(eventExpression.value.toString()))
        is VariableEventExpression -> yield(VariableTerm(eventExpression.variable.name))
        is ObjectEventExpression -> yieldAll(compile(eventExpression))
    }
}

private fun compile(objectExpression: ObjectEventExpression): Sequence<DictionaryTerm> = sequence {
    if (objectExpression.fields.isEmpty())
        yield(DictionaryTerm(VariableTerm("_"), emptyList()))
    else {
        val firstField = objectExpression.fields.first()
        val otherFields = objectExpression.fields.drop(1)

        // first expand the value of the first field
        compile(firstField.value).forEach { valueTerm ->
            // then compile and expand an object made from all the other fields
            val otherFieldsObject = ObjectEventExpression(otherFields)
            compile(otherFieldsObject).forEach { objectTerm ->
                // now combine the result
                val firstKeyValuePair = DictionaryTerm.KeyValuePair(atom(firstField.key.name), valueTerm)
                yield(DictionaryTerm(VariableTerm("_"), listOf(firstKeyValuePair) + objectTerm.pairs))
            }
        }
    }
}