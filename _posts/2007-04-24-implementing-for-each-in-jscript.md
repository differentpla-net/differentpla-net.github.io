---
title: "Implementing For Each in JScript"
date: 2007-04-24T11:01:30.000Z
x-drupal-nid: 176
x-needs-review: 2007-04-24T11:01:30.000Z
---
<pre>function forEach(enumerable, delegate)
{
    for (var enumerator = new Enumerator(enumerable); !enumerator.atEnd(); enumerator.moveNext())
    {
        delegate(enumerator.item());
    }
}</pre>

Used like this:

<pre>forEach(employees,
        function(employee)
        {
            WScript.Echo(employee.Salary);
        }
    );</pre>