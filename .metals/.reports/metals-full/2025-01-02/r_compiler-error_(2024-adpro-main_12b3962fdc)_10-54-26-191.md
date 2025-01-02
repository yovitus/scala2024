file://<HOME>/Documents/Datalogi/1.%20semester/Advanced%20Programming/Repo/2024-adpro-main/Main.scala
### java.lang.AssertionError: assertion failed: phase parser has already been used once; cannot be reused

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 9
uri: file://<HOME>/Documents/Datalogi/1.%20semester/Advanced%20Programming/Repo/2024-adpro-main/Main.scala
text:
```scala
object Ma@@

```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
	dotty.tools.dotc.core.Phases$Phase.init(Phases.scala:438)
	dotty.tools.dotc.core.Phases$Phase.init(Phases.scala:452)
	dotty.tools.dotc.core.Phases$PhasesBase.usePhases(Phases.scala:169)
	dotty.tools.dotc.core.Phases$PhasesBase.usePhases$(Phases.scala:38)
	dotty.tools.dotc.core.Contexts$ContextBase.usePhases(Contexts.scala:860)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:298)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:360)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:69)
	dotty.tools.dotc.Run.compileUnits(Run.scala:360)
	dotty.tools.dotc.Run.compileSources(Run.scala:261)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:161)
	dotty.tools.pc.MetalsDriver.run(MetalsDriver.scala:47)
	dotty.tools.pc.HoverProvider$.hover(HoverProvider.scala:38)
	dotty.tools.pc.ScalaPresentationCompiler.hover$$anonfun$1(ScalaPresentationCompiler.scala:363)
```
#### Short summary: 

java.lang.AssertionError: assertion failed: phase parser has already been used once; cannot be reused