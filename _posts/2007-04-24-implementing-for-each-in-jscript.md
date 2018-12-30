---
title: "Implementing For Each in JScript"
date: 2007-04-24T11:01:30.000Z
redirect_from: /content/2007/04/implementing-for-each-in-jscript
---

```
function forEach(enumerable, delegate)
{
    for (var enumerator = new Enumerator(enumerable); !enumerator.atEnd(); enumerator.moveNext())
    {
        delegate(enumerator.item());
    }
}
```

Used like this:

```
forEach(employees,
        function(employee)
        {
            WScript.Echo(employee.Salary);
        });
```
