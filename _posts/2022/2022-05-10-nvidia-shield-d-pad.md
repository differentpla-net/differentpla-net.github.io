---
title: "Flutter ListView selection on nVidia Shield"
date: 2022-05-10T09:55:00Z
tags: flutter
---

I'm messing around with Flutter on my nVidia Shield, and I needed to make "tapping" on ListView items work.

Wrap your app in a `Shortcuts` widget, as follows:

```dart
class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Shortcuts(
      shortcuts: {
        LogicalKeySet(LogicalKeyboardKey.select): const ActivateIntent()
      },
      child: const MaterialApp(
        title: 'Flutter Demo',
        home: MyHomePage(),
      ),
    );
  }
}
```
