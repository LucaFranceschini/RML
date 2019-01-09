package rml.parser

import rml.ast.*

fun buildSpecificationAst(ctx: rmlParser.SpecContext): Specification {
    val texpDecls = ctx.texpDecl().map(::buildDeclarationAst).toList()
    val evtypeDecls = ctx.evtypeDecl().map { it.accept(EvtypeDeclAstBuilder) }.toList()
    // assume the first declaration to be the main one
    return Specification(evtypeDecls, texpDecls, texpDecls[0].id)
}

fun buildEventTypeAst(ctx: rmlParser.EvtypeContext) = EventType(
        ctx.LOWERCASE_ID().text,
        ctx.simpleValues()?.simpleValue()?.map { it.accept(SimpleValueAstBuilder) }?.toList()
                ?: emptyList()
)

fun buildFieldAst(ctx: rmlParser.FieldContext): ObjectValue.Field =
        ObjectValue.Field(ctx.LOWERCASE_ID().text, ctx.value().accept(DataValueAstBuilder))

fun buildDeclarationAst(ctx: rmlParser.TexpDeclContext) = TraceExpDecl(
                TraceExpId(ctx.UPPERCASE_ID().text),
                visitVarsAux(ctx.vars()),
                ctx.texp().accept(TraceExpAstBuilder)
        )

// use different visitors because the result types are not the same

object EvtypeDeclAstBuilder: rmlBaseVisitor<EvtypeDecl>() {
    override fun visitDirectEvtypeDecl(ctx: rmlParser.DirectEvtypeDeclContext?) = DirectEvtypeDecl(
            buildEventTypeAst(ctx!!.evtype()),
            ctx.value().accept(DataValueAstBuilder))

    override fun visitDerivedEvtypeDecl(ctx: rmlParser.DerivedEvtypeDeclContext?) = DerivedEvtypeDecl(
            buildEventTypeAst(ctx!!.evtype().first()),
            ctx.evtype().drop(1).map(::buildEventTypeAst).toList()
    )
}

object DataValueAstBuilder: rmlBaseVisitor<DataValue>() {
    override fun visitSimpleVal(ctx: rmlParser.SimpleValContext?): SimpleValue =
            ctx!!.accept(SimpleValueAstBuilder)
    override fun visitObjectVal(ctx: rmlParser.ObjectValContext?) =
            ObjectValue(ctx!!.`object`().field().map(::buildFieldAst).toList())
    override fun visitListVal(ctx: rmlParser.ListValContext?) =
            ListValue(ctx!!.value()?.map { it.accept(this) } ?: emptyList())
    override fun visitOrPatternVal(ctx: rmlParser.OrPatternValContext?) =
            OrPatternValue(ctx!!.value(0).accept(this), ctx.value(1).accept(this))
}

object SimpleValueAstBuilder: rmlBaseVisitor<SimpleValue>() {
    override fun visitUnusedVal(ctx: rmlParser.UnusedValContext?) = UnusedValue
    override fun visitVarValue(ctx: rmlParser.VarValueContext?) = VarValue(ctx!!.LOWERCASE_ID().text)
    override fun visitIntValue(ctx: rmlParser.IntValueContext?) = IntValue(ctx!!.INT().text.toInt())
    override fun visitStringValue(ctx: rmlParser.StringValueContext?) =
            StringValue(ctx!!.text.removePrefix("'").removeSuffix("'"))
    override fun visitListValue(ctx: rmlParser.ListValueContext?) =
            ListSimpleValue(ctx!!.simpleValues()?.simpleValue()?.map { it.accept(this) } ?: emptyList())
}

object TraceExpAstBuilder: rmlBaseVisitor<TraceExp>() {
    override fun visitCatTExp(ctx: rmlParser.CatTExpContext?): ConcatTraceExp =
            visitBinTExp(ctx!!.texp(0), ctx.texp(1), ::ConcatTraceExp)

    override fun visitAndTExp(ctx: rmlParser.AndTExpContext?): AndTraceExp =
            visitBinTExp(ctx!!.texp(0), ctx.texp(1), ::AndTraceExp)

    override fun visitOrTExp(ctx: rmlParser.OrTExpContext?): OrTraceExp =
            visitBinTExp(ctx!!.texp(0), ctx.texp(1), ::OrTraceExp)

    override fun visitShufTExp(ctx: rmlParser.ShufTExpContext?): ShuffleTraceExp =
            visitBinTExp(ctx!!.texp(0), ctx.texp(1), ::ShuffleTraceExp)

    override fun visitFilterExp(ctx: rmlParser.FilterExpContext?) = FilterTraceExp(
            buildEventTypeAst(ctx!!.evtype()),
            ctx.texp().accept(this)
    )

    override fun visitEmptyTExp(ctx: rmlParser.EmptyTExpContext?): EmptyTraceExp = EmptyTraceExp

    override fun visitBlockTExp(ctx: rmlParser.BlockTExpContext?): BlockTraceExp =
            BlockTraceExp(
                    visitVarsAux(ctx!!.vars()),
                    ctx.texp().accept(this)
            )

    override fun visitVarTExp(ctx: rmlParser.VarTExpContext?): TraceExpVar =
            TraceExpVar(
                    TraceExpId(ctx!!.UPPERCASE_ID().text),
                    visitVarsAux(ctx.vars())
            )

    override fun visitEvtypeTExp(ctx: rmlParser.EvtypeTExpContext?) =
            EventTypeTraceExp(buildEventTypeAst(ctx!!.evtype()))

    override fun visitParTExp(ctx: rmlParser.ParTExpContext?): TraceExp =
            ctx!!.texp().accept(this)

    private fun <T: BinaryTraceExp> visitBinTExp(
            left: rmlParser.TexpContext,
            right: rmlParser.TexpContext,
            constructor: (TraceExp, TraceExp) -> T): T =
            constructor(left.accept(this), right.accept(this))
}

// visitVars already exists in BaseVisitor with the same signature, avoid confusion
private fun visitVarsAux(ctx: rmlParser.VarsContext?): List<VarId> =
        ctx?.LOWERCASE_ID()?.map { it.text }?.map(::VarId) ?: emptyList()