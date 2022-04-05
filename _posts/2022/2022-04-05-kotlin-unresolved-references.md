---
title: "Kotlin unresolved references in VS Code"
date: 2022-04-05T11:15:00Z
tags: kotlin flutter vscode
---

I'm attempting to write a Flutter plugin (written in Kotlin). If I open it in VS Code, I get a bunch of "unresolved reference" errors.

```
Unresolved reference: NonNull
Unresolved reference: io
Unresolved reference: FlutterPlugin
...
```

In the Kotlin output pane, you can see the following:

```
[Info  - 12:18:21] async0    Resolving dependencies for 'android' through Gradle's CLI using tasks [kotlinLSPProjectDeps]...
[Warn  - 12:18:23] async0    Gradle task failed:
FAILURE: Build failed with an exception.

* Where:
Initialization script 'C:\Users\roger\AppData\Local\Temp\classpath17101219706960277965.gradle' line: 49

* What went wrong:
Execution failed for task ':app:kotlinLSPProjectDeps'.
> Could not resolve all files for configuration ':app:debugCompileClasspath'.
   > Failed to transform libs.jar to match attributes {artifactType=android-classes-jar, org.gradle.libraryelements=jar, org.gradle.usage=java-runtime}.
      > Execution failed for JetifyTransform: C:\Users\roger\Source\foo\example\build\app\intermediates\foo\debug\libs.jar.
         > Transform's input file does not exist: C:\Users\roger\Source\foo\example\build\app\intermediates\flutter\debug\libs.jar. (See https://issuetracker.google.com/issues/158753935)
```

The fix seems to be to run the following:

```
cd example
flutter build apk --debug
flutter build apk --release     # optional
flutter build apk --profile     # optional
```

The last two are optional, but resolve some other noise in the output pane.

Then press Ctrl+Shift+P, and run the "Kotlin: Restart the Language Server" command.
