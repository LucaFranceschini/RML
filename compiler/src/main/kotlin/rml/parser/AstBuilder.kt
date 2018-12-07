package rml.parser

import rml.ast.*

fun buildSpecificationAst(ctx: rmlParser.SpecContext): Specification =
        Specification(ctx.decl().map(::buildDeclarationAst).toList())

fun buildDeclarationAst(ctx: rmlParser.DeclContext): Declaration =
        Declaration(
                TraceExpId(ctx.UPPERCASE_ID().text),
                visitVarsAux(ctx.vars()),
                ctx.texp().accept(TraceExpAstBuilder)
        )

// use two different visitors because the result type is not the same

object TraceExpAstBuilder: rmlBaseVisitor<TraceExp>() {
    override fun visitCatTExp(ctx: rmlParser.CatTExpContext?): ConcatTraceExp =
            visitBinTExp(ctx!!.texp(0), ctx.texp(1), ::ConcatTraceExp)

    override fun visitAndTExp(ctx: rmlParser.AndTExpContext?): AndTraceExp =
            visitBinTExp(ctx!!.texp(0), ctx.texp(1), ::AndTraceExp)

    override fun visitOrTExp(ctx: rmlParser.OrTExpContext?): OrTraceExp =
            visitBinTExp(ctx!!.texp(0), ctx.texp(1), ::OrTraceExp)

    override fun visitShufTExp(ctx: rmlParser.ShufTExpContext?): ShuffleTraceExp =
            visitBinTExp(ctx!!.texp(0), ctx.texp(1), ::ShuffleTraceExp)

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

    override fun visitEvtypeTExp(ctx: rmlParser.EvtypeTExpContext?): EventTypeTraceExp =
            EventTypeTraceExp(
                    EventTypeTraceExp.Id(ctx!!.evtype().LOWERCASE_ID().text),
                    ctx.evtype().terms().term().map { it.accept(TermAstBuilder) }.toList()
            )

    override fun visitParTExp(ctx: rmlParser.ParTExpContext?): TraceExp =
            ctx!!.texp().accept(this)

    private fun <T: BinaryTraceExp> visitBinTExp(
            left: rmlParser.TexpContext,
            right: rmlParser.TexpContext,
            constructor: (TraceExp, TraceExp) -> T): T =
            constructor(left.accept(this), right.accept(this))
}

object TermAstBuilder: rmlBaseVisitor<Term>() {
    override fun visitVarTerm(ctx: rmlParser.VarTermContext?): VarTerm = VarTerm(VarId(ctx!!.text))
    override fun visitIntTerm(ctx: rmlParser.IntTermContext?): IntTerm = IntTerm(ctx!!.text.toInt())
    override fun visitStringTerm(ctx: rmlParser.StringTermContext?): StringTerm = StringTerm(ctx!!.text)
}

// visitVars already exists in BaseVisitor with the same signature, avoid confusion
private fun visitVarsAux(ctx: rmlParser.VarsContext?): List<VarId> =
        ctx?.LOWERCASE_ID()?.map { it.text }?.map(::VarId) ?: emptyList()