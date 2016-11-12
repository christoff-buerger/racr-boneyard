# Experimental *Kawa* adaptation of *RACR*

The `core.scm` contained in this *zip* archive is an **experimental** adaptation of *RACR's* `(racr core)` library for [*Kawa*](https://www.gnu.org/software/kawa/). It is **not** compiling with *Kawa 2.1*, the most recent version on the 12th of November 2016. Compilation aborts with an exception, indicating an internal implementation error of *Kawa*:

```
Exception in thread "main" java.lang.InternalError
	at gnu.bytecode.Label.setTypes(Label.java:106)
	at gnu.bytecode.Label.define(Label.java:231)
	at gnu.bytecode.CodeAttr.emitFi(CodeAttr.java:1934)
	at gnu.expr.IfExp.compile(IfExp.java:145)
	at gnu.expr.IfExp.compile(IfExp.java:54)
	at gnu.expr.Expression.compileWithPosition(Expression.java:150)
	at gnu.expr.Expression.compileWithPosition(Expression.java:126)
	at gnu.expr.IfExp.compile(IfExp.java:125)
	at gnu.expr.IfExp.compile(IfExp.java:54)
	at gnu.expr.Expression.compile(Expression.java:166)
	at gnu.expr.SetExp.compile(SetExp.java:272)
	at gnu.expr.Expression.compileWithPosition(Expression.java:150)
	at gnu.expr.Expression.compileWithPosition(Expression.java:126)
	at gnu.expr.BeginExp.compile(BeginExp.java:146)
	at gnu.expr.Expression.compileWithPosition(Expression.java:143)
	at gnu.expr.Expression.compileWithPosition(Expression.java:126)
	at gnu.expr.LetExp.compile(LetExp.java:196)
	at gnu.expr.Expression.compileWithPosition(Expression.java:143)
	at gnu.expr.Expression.compileWithPosition(Expression.java:126)
	at gnu.expr.BeginExp.compile(BeginExp.java:154)
	at gnu.expr.Expression.compileWithPosition(Expression.java:143)
	at gnu.expr.LambdaExp.compileBody(LambdaExp.java:1689)
	at gnu.expr.LambdaExp.compileAsMethod(LambdaExp.java:1667)
	at gnu.expr.LambdaExp.compileSetField(LambdaExp.java:704)
	at gnu.expr.SetExp.compile(SetExp.java:169)
	at gnu.expr.Expression.compileWithPosition(Expression.java:143)
	at gnu.expr.Expression.compileWithPosition(Expression.java:126)
	at gnu.kawa.functions.AppendValues.compile(AppendValues.java:51)
	at gnu.expr.ApplyExp.inlineCompile(ApplyExp.java:715)
	at gnu.expr.ApplyExp.compile(ApplyExp.java:245)
	at gnu.expr.ApplyExp.compile(ApplyExp.java:191)
	at gnu.expr.Expression.compileWithPosition(Expression.java:143)
	at gnu.expr.LambdaExp.compileBody(LambdaExp.java:1689)
	at gnu.expr.Compilation.generateBytecode(Compilation.java:2297)
	at gnu.expr.Compilation.process(Compilation.java:2172)
	at gnu.expr.ModuleInfo.loadByStages(ModuleInfo.java:302)
	at gnu.expr.ModuleInfo.loadByStages(ModuleInfo.java:296)
	at gnu.expr.ModuleInfo.loadByStages(ModuleInfo.java:284)
	at kawa.repl.compileFiles(repl.java:753)
	at kawa.repl.processArgs(repl.java:434)
	at kawa.repl.main(repl.java:793)
```

The adaptations done are:
 * transformation of *R6RS* library syntax to *R7RS/Kawa* syntax
 * transformation of *R6RS* record definitions to *SRFI 9* records (as supported by *Kawa*)
 * uncommenting of `with-specification` definition (required because *Kawa's* `define-syntax` implementation is incomplete)
 * split of `specify-ag-rule` and separation of its nested `specify-attribute*` macro in an own definition (again, because *Kawa's* `define-syntax` was erroneous)
 * deletion of everything related to pattern attributes (to reduce necessary adaptations)

