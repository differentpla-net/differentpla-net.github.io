---
title: "Flutter/Kotlin/Gradle Unsupported class file in VS Code"
date: 2022-04-05T09:17:00Z
tags: kotlin flutter gradle vscode
---

Opening a Flutter/Dart/Kotlin project in VS Code on Windows, and a bunch of stuff is broken.

Gradle for Java:
> There is no valid JAVA_HOME setting to launch Gradle Language Server. Please check your "java.jdt.ls.java.home" setting.

Kotlin (fwcd):
> The Kotlin Language Client server crashed 5 times in the last 3 minutes. The server will not be restarted.

...and in the Output pane:

> java.lang.UnsupportedClassVersionError: org/javacs/kt/MainKt has been compiled by a more recent version of the Java Runtime (class file version 55.0), this version of the Java Runtime only recognizes class file versions up to 52.0

The Kotlin compiler used by the Kotlin extension seems to have been compiled with Java 11; I've got Java 8 installed. Somewhat oddly, the <https://www.java.com/en/download/> page only lists Java 8, despite the most-recent version being 18, according to <https://dev.java/>. That's ... quite a difference.

As far as I can tell, Java 8 is still kinda-supported -- see <https://endoflife.date/java>, but it was the last version to use the old licensing model, so Oracle doesn't automatically push you to anything newer. I should probably just uninstall it, and install Java 17 -- the most recent LTS version -- instead.

As it happens, I've got JDK 16 installed, but if I set `JAVA_HOME` and `JDK_HOME` appropriately, the Kotlin extension fails with something different:

```
[Warn  - 10:25:22] async0    Gradle task failed:
FAILURE: Build failed with an exception.

* What went wrong:
Could not open init generic class cache for initialization script 'C:\Users\roger\AppData\Local\Temp\classpath16853392236147141051.gradle' (C:\Users\roger\.gradle\caches\6.7\scripts\j5bb3grjsvxg66ch1zymccc2).
> BUG! exception in phase 'semantic analysis' in source unit '_BuildScript_' Unsupported class file major version 60
```

Using Google to search for that error leads me to <https://stackoverflow.com/questions/67079327/how-can-i-fix-unsupported-class-file-major-version-60-in-intellij-idea>, but that's for IntelliJ IDEA, and I'm using Visual Studio Code.

[One of the answers](https://stackoverflow.com/a/67955806) to that question gives a clue:

Edit `android\gradle\wrapper\gradle-wrapper.properties`, and change the last line as follows:

```
distributionUrl=https\://services.gradle.org/distributions/gradle-7.4.2-all.zip
```
