file://<HOME>/Documents/Datalogi/1.%20semester/Advanced%20Programming/Repo/2024-adpro-main/Main.scala
### java.lang.UnsupportedOperationException: tail of empty list

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
scala.collection.immutable.Nil$.tail(List.scala:664)
	scala.collection.immutable.Nil$.tail(List.scala:661)
	dotty.tools.dotc.core.Types$Type.findMember(Types.scala:953)
	dotty.tools.dotc.core.Types$Type.memberBasedOnFlags(Types.scala:729)
	dotty.tools.dotc.core.Types$Type.nonPrivateMember(Types.scala:719)
	dotty.tools.dotc.core.Types$typeAliasNameFilter$.apply(Types.scala:6868)
	dotty.tools.dotc.core.Types$.dotty$tools$dotc$core$Types$Type$$_$memberNames$$anonfun$1(Types.scala:967)
	scala.collection.immutable.BitmapIndexedSetNode.filterImpl(HashSet.scala:1048)
	scala.collection.immutable.HashSet.filterImpl(HashSet.scala:329)
	scala.collection.immutable.HashSet.filterImpl(HashSet.scala:34)
	scala.collection.StrictOptimizedIterableOps.filter(StrictOptimizedIterableOps.scala:218)
	scala.collection.StrictOptimizedIterableOps.filter$(StrictOptimizedIterableOps.scala:218)
	scala.collection.immutable.HashSet.filter(HashSet.scala:34)
	dotty.tools.dotc.core.Types$Type.memberNames(Types.scala:967)
	dotty.tools.dotc.core.Types$Type.memberDenots(Types.scala:983)
	dotty.tools.dotc.core.Types$Type.typeAliasMembers(Types.scala:1037)
	dotty.tools.dotc.printing.PlainPrinter.printWithoutPrefix$lzyINIT1(PlainPrinter.scala:148)
	dotty.tools.dotc.printing.PlainPrinter.printWithoutPrefix(PlainPrinter.scala:148)
	dotty.tools.dotc.printing.PlainPrinter.toText$$anonfun$1(PlainPrinter.scala:196)
	dotty.tools.dotc.printing.MessageLimiter.controlled(MessageLimiter.scala:23)
	dotty.tools.dotc.printing.PlainPrinter.controlled(PlainPrinter.scala:39)
	dotty.tools.dotc.printing.PlainPrinter.toText(PlainPrinter.scala:324)
	dotty.tools.dotc.printing.RefinedPrinter.toText$$anonfun$1(RefinedPrinter.scala:287)
	dotty.tools.dotc.printing.MessageLimiter.controlled(MessageLimiter.scala:23)
	dotty.tools.dotc.printing.PlainPrinter.controlled(PlainPrinter.scala:39)
	dotty.tools.dotc.printing.RefinedPrinter.toText(RefinedPrinter.scala:323)
	dotty.tools.pc.printer.ShortenedTypePrinter.toText(ShortenedTypePrinter.scala:206)
	dotty.tools.pc.printer.ShortenedTypePrinter.tpe(ShortenedTypePrinter.scala:213)
	dotty.tools.pc.printer.ShortenedTypePrinter.expressionType(ShortenedTypePrinter.scala:88)
	dotty.tools.pc.HoverProvider$.hover(HoverProvider.scala:132)
	dotty.tools.pc.ScalaPresentationCompiler.hover$$anonfun$1(ScalaPresentationCompiler.scala:363)
```
#### Short summary: 

java.lang.UnsupportedOperationException: tail of empty list