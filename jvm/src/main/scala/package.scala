package psp
package std

import api._
import javax.tools._, Diagnostic.Kind, JavaFileManager.Location
import StdShow._
import psp.std.token.Keyword

package jvm {
  object opcodes {
    private def codes(p: ToBool[Opcode]): Direct[Opcode] = Opcode.values filter p toDirect

    def all        = Opcode.values.toDirect
    def constant   = codes(_.isConstant)
    def load       = codes(_.isLoad)
    def store      = codes(_.isStore)
    def stack      = codes(_.isStack)
    def math       = codes(_.isMath)
    def conversion = codes(_.isConversion)
    def comparison = codes(_.isComparison)
    def control    = codes(_.isControl)
    def reference  = codes(_.isReference)
    def extended   = codes(_.isExtended)
    def reserved   = codes(_.isReserved)
  }

  trait StdClasses {
    def internalNameOf[A: CTag](): String                = javaNameOf[A].replace('.', '/')
    def javaNameOf[A: CTag](): String                    = classOf[A].getName
    def methodsOf[A: CTag](): Direct[jMethod]            = classOf[A].getMethods.toDirect filterNot objectMethods
    def declsOf[A: CTag](): Direct[jMethod]              = classOf[A].getDeclaredMethods.toDirect
    def bridgeMethodsOf[A: CTag](): Direct[jMethod]      = methodsOf[A] filter (_.isBridge)
    def specializedMethodsOf[A: CTag](): Direct[jMethod] = methodsOf[A] filter (_.isSpecialized)
    def publicMethodsOf[A: CTag](): Direct[jMethod]      = methodsOf[A] filter (x => x.isPublic && !x.isBridge && !x.isSpecialized)
    def staticMethodsOf[A: CTag](): Direct[jMethod]      = publicMethodsOf[A] filter (_.isStatic)
    def instanceMethodsOf[A: CTag](): Direct[jMethod]    = publicMethodsOf[A] filterNot (_.isStatic)

    lazy val objectMethods: ToBool[jMethod] = classOf[Object].getMethods.byEquals.contains

    def contextLoader(): ClassLoader      = noNull(currentThread.getContextClassLoader, nullLoader)
    def contextLoaderUris: Each[jUri]     = contextLoaders flatMap (_.uris)
    def contextLoaders: Each[ClassLoader] = contextLoader.parentChain

    // def loadersIn(path: Path): pVector[PolicyLoader] = jarsIn(path) map newLoader
    // def newLoader(path: Path): PolicyLoader          = jio.JioJar(path).loader
  }
}

package object jvm extends psp.std.jvm.StdClasses {
  import JvmConstants._

  final val JavaVoid     = java.lang.Void.TYPE
  final val JavaObject   = classOf[java.lang.Object]
  final val ScalaNothing = classOf[scala.runtime.Nothing$]
  final val ScalaNull    = classOf[scala.runtime.Null$]

  val sPartialFunction = jTypeArgExtractor("""scala[.]PartialFunction(\d+)""".r)
  val sFunction        = jTypeArgExtractor("""scala[.]Function(\d+)""".r)
  val sProduct         = jTypeArgExtractor("""scala[.]Product(\d+)""".r)
  val sTuple           = jTypeArgExtractor("""scala[.]Tuple(\d+)""".r)

  implicit def opsMethod(x: jMethod): jMethodOps                                                    = new jMethodOps(x)
  implicit def opsJType(x: jType): jTypeOps                                                         = new jTypeOps(x)
  implicit def opsClass[A](x: Class[A]): jClassOps[A]                                               = new jClassOps[A](x)
  implicit def opsParameterizedType(x: jParameterizedType): jParameterizedTypeOps                   = new jParameterizedTypeOps(x)
  implicit def opsJTypeVariable[D <: jGenericDeclaration](x: jTypeVariable[D]): jTypeVariableOps[D] = new jTypeVariableOps[D](x)

  implicit def showPoolEntry: Show[PoolEntry]           = Show(_.content)
  implicit def showInnerClassInfo: Show[InnerClassInfo] = Show(_.nestString)
  implicit def jTypeSeqShow: Show[scSeq[jType]]         = Show(_.m.inParens)
  implicit def jTypeShow: Show[jType]                   = Show(_.scalaString)
  implicit def jMethodShow: Show[jMethod]               = Show(_.scalaString)

  implicit def methodTreeShow: Show[com.sun.source.tree.MethodTree] = Show(t => ("" + t).replaceAll("\\s+", " ").truncateTo(80.size))

  // implicit def ShowJvmDescriptor[A <: Descriptor]: Show[A] = Show[A] {
  //   case MethodDescriptor(params, restpe) => (params map (_.doc)).inParens <+> "=>" <+> restpe render
  //   case ClassDescriptor(desc)            => desc
  //   case ArrayDescriptor(elem, dim)       => "%s%s" % (elem.doc, "[]" * dim asis) render
  //   case x: Tag                           => x.javaType
  //   case NoDescriptor                     => "<none>"
  // }

  def c[A](implicit tag: CTag[A]): jClassOps[A] = new jClassOps[A](tag.runtimeClass.castTo[Class[A]])

  implicit class DiagnosticOps[A](x: Diagnostic[A]) {
    import Diagnostic.Kind._
    def carat  = x.getPosition
    def code   = x.getCode
    def column = Nth(x.getColumnNumber.toInt)
    def end    = x.getEndPosition
    def kind   = x.getKind
    def line   = Nth(x.getLineNumber.toInt)
    def source = x.getSource
    def start  = x.getStartPosition
    def range  = indexRange(x.getStartPosition.toInt, x.getEndPosition.toInt)

    def isError   = kind == ERROR
    def isWarning = kind == MANDATORY_WARNING || kind == WARNING
    def isInfo    = !isError && !isWarning
  }

  type LazyPairs = Array[Lazy[ElementValuePair]]
  def lazyBytes(elem: ElementValue): LazyPairs = Array(Lazy(ElementValuePair("bytes", elem)))

  final val LBracket             = '\u005b'
  val ScalaSigAnnotationName     = JvmName("Lscala/reflect/ScalaSignature;")
  val ScalaLongSigAnnotationName = JvmName("Lscala/reflect/ScalaLongSignature;")
  val TraitConstructor           = JvmName("$init$")
  val ClassConstructor           = JvmName("<init>")
  val StaticConstructor          = JvmName("<clinit>")

  // implicit val emptyJvmName   = Empty[JvmName](JvmName(""))
  // implicit val emptySignature = Empty[JvmSignature](JvmSignature(""))
  implicit val emptyCode      = Empty[Code](Code(0, 0, Array(), Array(), Array())(null))

  def NoJvmName: JvmName = ??? //   = emptyJvmName.empty
  // def NoSignature = emptySignature.empty
  def NoCode: Code = ??? //      = emptyCode.empty

  implicit def attributeInfoEq: Eq[AttributeInfo]  = Eq.natural()
  implicit def ShowJvmMemberInfo: Show[MemberInfo] = Show(_.codeString)
  // implicit def showClassInfo: Show[ClassInfo]      = Show[ClassInfo] { info =>
  //   import info._
  //   def interfaces_s = interfaces.m opt (xs => "implements" <+> xs.joinComma)
  //   def fields_s     = fields.m.joinLines.indent()
  //   def methods_s    = methods.m.joinLines.indent()
  //   def attrs_s      = attributes.m opt (_.joinComma)
  //   def summary: Doc = "".asis // flags.classFlags.doc <+> keyword <+> name <+> "extends" <+> superName <+> interfaces_s

  //   // s"""|$summary
  //   //     |Constant pool:
  //   //     |${info.poolString}
  //   //     |
  //   //     |$fields_s$methods_s$attrs_s
  //   //     |""".stripMargin
  // }

  implicit def showKeyword: Show[Keyword] = Show[Keyword] {
    case Keyword.Empty            => ""
    case Keyword.ClassConstructor => ""
    case Keyword.ValueParameter   => ""
    case Keyword.TypeParameter    => ""
    case Keyword.Constructor      => "def this"
    case Keyword.PackageObject    => "package object"
    case Keyword.CaseObject       => "case object"
    case Keyword.CaseClass        => "case class"
    case k                        => k.toString.toLowerCase
  }
  implicit def ShowAccess        = Show[Access](_.toString.toLowerCase)
  implicit def ShowJvmName       = Show[JvmName](_.decoded)
  implicit def ShowJvmRef        = Show[JvmRef](ref => s"def ${ref.name} ${ref.descriptor}")
  implicit def ShowJvmCall       = Show[JvmCall](call => s"${call.from} calls ${call.to}")
  implicit def ShowAttributeInfo = Show[AttributeInfo](attr => s"${attr.name}")

  implicit class AttributeInfoOps(val info: AttributeInfo) extends AnyVal {
    def name: String = info match {
      case attr.Generic(name, _) => name
      case _                     => info.shortClass stripSuffix "$"
    }
  }

  implicit class InnerClassInfoOps(info: InnerClassInfo) {
    import info._
    def isEntryOfEnclosingClass = !isAnonymousClass && (innerClass == thisClass)
    def isEntryOfNestedClass    = !isAnonymousClass && (outerClass == thisClass)
    def isTopLevelClass         = outerClass.isEmpty
    def isAnonymousClass        = innerName.isEmpty
    def isMemberClass           = !isTopLevelClass

    def kind = (
      if (isEntryOfEnclosingClass) "inner/enclosing"
      else if (isEntryOfNestedClass) "inner/nested"
      else if (isAnonymousClass) "inner/anon"
      else "inner"
    )
    def nestString = (
      if (isEntryOfEnclosingClass) s"enclosing class: $outerClass"
      else if (isEntryOfNestedClass) s"member class: $innerClass"
      else if (isAnonymousClass) s"anonymous class: $innerClass"
      else s"$innerClass in $outerClass"
    )
  }
  implicit class HasIsEmptyOps[A](val x: HasIsEmpty[A]) extends AnyVal {
    private def self: A                   = x.castTo[A]
    def fold[B](zero: => B)(f: A => B): B = if (x.isEmpty) zero else f(self)
    def orElse[A1 >: A](alt: => A1): A1   = if (x.isEmpty) alt else self
    def nonEmpty                          = !x.isEmpty
  }
  implicit class HasJvmFlagsOps(val entity: HasJvmFlags) extends AnyVal {
    import entity._
    def access: Access = (
      if (flags.isPublic) Access.Public
      else if (flags.isProtected) Access.Protected
      else if (flags.isPrivate) Access.Private
      else Access.Public
    )
  }
  implicit class HasAttributesOps(val entity: HasAttributes) extends AnyVal {
    import entity._
    // def signature: JvmSignature                 = attributes zfirst  { case attr.Signature(sig)                    => JvmSignature(sig) }
    def innerClasses: Array[InnerClassInfo]     = attributes zfirst  { case attr.InnerClasses(xs)                  => xs                }
    def annotations: Array[Lazy[JvmAnnotation]] = attributes zfirst  { case attr.RuntimeVisibleAnnotations(annots) => annots            }
    def code: Code                              = attributes zfirst  { case attr.Code(x)                           => x                 }
    def exceptions: sciVector[String]           = attributes zfirst  { case attr.Exceptions(xs)                    => xs.toScalaVector  }
    def constantValue: Option[Any]              = attributes first   { case attr.ConstantValue(x)                  => x                 }
  }
  implicit class ClassInfoOps(val info: ClassInfo) extends AnyVal {
    import info._
    def isInterface = flags.isInterface
    def keyword     = flags.classKeyword

    def memberNamed(name: JvmName) = members find (_.name == name)

    def members           = (fields ++ methods.m) // sortOrder (_.toString) XXX
    def memberDescriptors = members map (_.toErasedString)
    def memberSignatures  = members collect { case x if x.hasSignature => x.toGenericString }
    def memberCodes       = members collect { case x if x.hasCode      => x.codeString }
    // def descriptorsString = memberDescriptors.joinLines mapNonEmpty ("\n-- Member Descriptors --\n" + _ + "\n")
    // def signaturesString  = memberSignatures.joinLines mapNonEmpty ("\n-- Member Signatures --\n" + _ + "\n")
    // def codesString       = memberCodes.joinLines mapNonEmpty ("\n-- Member Code --\n" + _ + "\n")
    // def innersString      = info.innerClasses.m.joinLines mapNonEmpty ("\n-- Inner Classes --\n" + _ + "\n")
    // def membersString     = descriptorsString <> signaturesString <> codesString
    // def extendsString     = superName.decoded mapNonEmpty " extends ".+
    // def implementsString  = interfaces.m.joinLines mapNonEmpty "Implements: ".+

    def poolString: String = {
      import pool._
      def valueAt(index: UShort): String = valueOfEntry(poolEntries(index.toInt))
      def valueOfEntry(entry: PoolEntry): String = entry match {
        case Utf8_info("<init>")          => "\"<init>\""   // so faithful to javap
        case x: DirectEntry               => "" + x.value
        case Ref_Info(idx1, idx2)         => "%s.%s".format(valueAt(idx1), valueAt(idx2))
        case NameAndType_info(idx1, idx2) => "%s:%s".format(valueAt(idx1), valueAt(idx2))
        case Class_info(index)            => valueAt(index)
        case String_info(index)           => valueAt(index)
        case _                            => "???"
      }
      def line(entry: PoolEntry, number: Int): String = {
        val comment = entry match {
          case _: DirectEntry => ""
          case _              => " //  " + valueOfEntry(entry)
        }
        "%6s = %-18s %-15s%s".format("#" + number, entry.label, entry.content.sanitize, comment)
      }

      poolEntries.tail zip (Indexed from 1) map line mkString "\n"
    }

    // private def group(label: String, xs: scTraversable[(String, String)]) =
    //   xs map { case (name, value) => line(label, name, value) } mkString "\n"

    // private def line(label: String, name: String, data: String) =
    //   "  %-15s  %30s  %s".format(label, name, data).trimTrailing

    override def toString = Try(composeString).fold(e => { echoErr(s"Fail: $e\nIn: $name") ; throw e }, x => x)

    // private def callsString = {
    //   val calls = members filter (_.hasCode) flatMap (m => m.code.methodCalls map (c => (m, c)))
    //   calls map { case (m, c) =>
    //     val n = s"$name.${m.name.decoded}"
    //     f"  call  $n%-50s    ${c.decodedFullName}".trimTrailing
    //   } mkString "\n"
    // }
    def composeString = (
      ""
      // sciList(
      //   // XXX the last place forcing our lazies
      //   // group("annotation", annotations map (x => (x.tpe, sanitize(x.args.mkString(", "))))),
      //   s"$keyword $name$extendsString",
      //   if (info.signature.isEmpty) "" else line("class sig", "", info.signature.toString),
      //   group("interface", interfaces map (x => "" -> x.toString) seq),
      //   (info.innerClasses.seq map (ic => line(ic.kind, ic.innerName.toString, ic.nestString))).sorted.mkString("\n"),
      //   group("descriptor", members map (x => x.name.toString -> x.descriptor.toString) seq),
      //   group("signature", members filter (_.hasSignature) map (x => x.name.toString -> x.signature.toString) seq),
      //   callsString
      // ) map (_.trimTrailing) filterNot (_ == "") mkString ("", "\n", "\n")
    )
  }

  implicit class MemberInfoOps(val member: MemberInfo) extends AnyVal {
    import member._

    def className                                    = ref.className
    def descriptor                                   = ref.descriptor
    // def signatureOrDescriptor: SignatureOrDescriptor = member.signature orElse descriptor

    def isField             = ref.isField
    def isMethod            = ref.isMethod
    def isBridgeMethod      = isMethod && flags.isBridge
    def isTraitConstructor  = isMethod && name == TraitConstructor
    def isClassConstructor  = isMethod && name == ClassConstructor
    def isStaticConstructor = isMethod && name == StaticConstructor
    def scalaFlags          = toScalaMethodFlags(flags)
    def hasSignature = false //        = member.signature.nonEmpty
    def hasCode             = member.code.nonEmpty
    def toErasedString = "" //      = "%-30s %s".format(member.name.decoded, descriptor)
    def toGenericString = "" //     = "%-30s %s".format(member.name.decoded, member.signature)

    def fieldAccesses = member.code.fieldAccesses
    def methodCalls   = member.code.methodCalls
    def accesses      = member.code.accesses

    def codeString    = ""
    // "bytes=%d, stack=%d, locals=%d, args_size=%s".format(
    //   member.code.size,
    //   member.code.max_stack.toInt,
    //   member.code.max_locals.toInt,
    //   parameterCount
    // )
    def parameterCount = ref.arity + ( if (isMethod && !member.flags.isStatic) 1 else 0 )
  }

  implicit class JvmNameOps(x: JvmName) {
    import x.name
    def isEmpty             = name == ""
    def length              = name.length
    def endsWith(s: String) = name endsWith s
    def contains(s: String) = name contains s
    def contains(ch: Char)  = name containsChar ch
    def decoded: String     = decodeName(name)

    def simpleName: JvmName = JvmName(name.dottedSegments.last)
    def packageName: JvmName = if (x == simpleName) NoJvmName else JvmName(name.dottedSegments.init mkString ".")
  }
  implicit class JvmCallOps(val call: JvmCall) extends AnyVal {
    import call._
    def swap: JvmCall      = JvmCall(to, from)
    def isSamePackage      = from.packageName == to.packageName
    def isSameClass        = from.className == to.className
    def isRecursive        = isSameClass && (from.descriptor == to.descriptor)
    def isBetweenOverloads = isSameClass && !isRecursive && (from.name == to.name)
  }
  implicit class JvmRefOps(val ref: JvmRef) extends AnyVal {
    import ref._

    // def paramTypes: Direct[Descriptor] = descriptor match {
    //   case MethodDescriptor(ps, _) => ps
    //   case _                       => Direct()
    // }
    // def returnType: Descriptor = descriptor match {
    //   case MethodDescriptor(_, restpe) => restpe
    //   case _                           => descriptor
    // }
    def packageName      = className.packageName
    def decodedClassName = className.decoded
    def decodedName      = name.decoded
    def decodedFullName  = decodeName(fullName)
    def fullName = ???
    def fullDecodedName = ???
    // def fullName         = s"$className$SelectorString$name"
    // def fullDecodedName  = s"$decodedClassName$SelectorString$decodedName"
    def isMethod         = descriptor.isMethod
    def isField          = !isMethod
    def arity            = 0 // paramTypes.size

    // override def toString = JvmRef.refFormat(ref)
  }

  implicit def showElementValue: Show[ElementValue] = Show[ElementValue] {
    case attr.ConstantFloat(x)             => s"Constant($x: Float)"
    case attr.ConstantDouble(x)            => s"Constant($x: Double)"
    case attr.ConstantLong(x)              => s"Constant($x: Long)"
    case attr.StringConstValue(x)          => s"""String("$x")"""
    case attr.EnumConstValue(tp, name)     => s"Enum($name: $tp)"
    case attr.AnnotationNestedValue(annot) => s"@($annot)"
    case attr.ClassInfoValue(name)         => s"classOf[$name]"
    case attr.AnnotationArrayValue(values) => "Array(%s)".format(values mkString ", ")
    case attr.ConstantInteger(x, tag)      => "" // s"Constant($x: ${Tag(tag).javaType})"
  }
  implicit def showJvmAnnotation: Show[JvmAnnotation] = Show[JvmAnnotation] {
    case attr.MarkerAnnotation(tpe)         => s"@$tpe"
    case attr.ScalaSigAnnotation(elem)      => "@ScalaSig(?)"
    case attr.ScalaLongSigAnnotation(elem)  => "@ScalaLongSig(?)"
    case attr.AnnotationWithArgs(tpe, args) => s"@$tpe(%s)".format(args mkString ", ")
  }

  implicit def JvmNameOrdering: Order[JvmName]         = orderBy[JvmName](_.decoded)
  implicit def JvmInfoOrdering[A <: JvmInfo]: Order[A] = orderBy[A](_.name)
  implicit def liftJvmName(chars: String): JvmName     = JvmName(chars)

  implicit class OpcodeOps(val op: Opcode) extends AnyVal {
    private def within(lo: Int, hi: Int): Boolean = lo <= op.getValue && op.getValue <= hi

    def isConstant   = within(0, 20)
    def isLoad       = within(21, 53)
    def isStore      = within(54, 86)
    def isStack      = within(87, 95)
    def isMath       = within(96, 132)
    def isConversion = within(133, 147)
    def isComparison = within(148, 166)
    def isControl    = within(167, 177)
    def isReference  = within(178, 195)
    def isExtended   = within(196, 201)
    def isReserved   = within(202, 255)
  }
}
