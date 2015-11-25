addSbtPlugin("org.improving" % "psp-libsbt" % sys.props.getOrElse("libsbt.version", "0.5.9"))

resolvers += Resolver.url("paulp/sbt-plugins", url("https://dl.bintray.com/paulp/sbt-plugins"))(Resolver.ivyStylePatterns)
