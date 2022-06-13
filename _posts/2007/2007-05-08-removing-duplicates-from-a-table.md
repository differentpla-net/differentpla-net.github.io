---
title: "Removing duplicates from a table"
date: 2007-05-08T12:21:37.000Z
tags: sql
---
Let's assume that you've got a table:

```sql
CREATE TABLE #t (
  Id INT IDENTITY,
  Name VARCHAR(50),
  Number INT )
```

... into which you've accidentally inserted some duplicate rows:

```sql
INSERT INTO #t(Name, Number) VALUES('Alice', 3)
INSERT INTO #t(Name, Number) VALUES('Alice', 3)
INSERT INTO #t(Name, Number) VALUES('Alice', 3)
INSERT INTO #t(Name, Number) VALUES('Alice', 4)
INSERT INTO #t(Name, Number) VALUES('Bob', 1)
INSERT INTO #t(Name, Number) VALUES('Bob', 2)
INSERT INTO #t(Name, Number) VALUES('Bob', 3)
INSERT INTO #t(Name, Number) VALUES('Charlie', 1)
INSERT INTO #t(Name, Number) VALUES('Charlie', 2)
INSERT INTO #t(Name, Number) VALUES('Charlie', 3)
INSERT INTO #t(Name, Number) VALUES('Charlie', 4)
INSERT INTO #t(Name, Number) VALUES('David', 1)
INSERT INTO #t(Name, Number) VALUES('David', 1)
INSERT INTO #t(Name, Number) VALUES('David', 2)
INSERT INTO #t(Name, Number) VALUES('David', 2)
```

(The [names](http://en.wikipedia.org/wiki/Alice_and_Bob) are those used when discussing crypto.)

As you can see, I've managed to insert two duplicate rows for Alice, and two duplicate (but different) rows for David.

It's pretty easy to find the non-duplicates, by doing the following:

```sql
SELECT MIN(Id) AS Id, Name, Number FROM #t
GROUP BY Name, Number
ORDER BY MIN(Id)
```

Actually deleting them, on the other hand, is more tricky:

```c#
DELETE #t FROM
	( SELECT MIN(L.Id) AS MinId, L.Name, L.Number FROM #t AS L
		INNER JOIN #t AS R
		ON L.Name = R.Name AND L.Number = R.Number
		AND L.Id <> R.Id
		GROUP BY L.Name, L.Number ) AS Q
INNER JOIN #t
	ON #t.Id > Q.MinId
	AND #t.Name = Q.Name
	AND #t.Number = Q.Number
```

The inner portion...

```c#
SELECT MIN(L.Id) AS MinId, L.Name, L.Number FROM #t AS L
	INNER JOIN #t AS R
	ON L.Name = R.Name AND L.Number = R.Number
	AND L.Id <> R.Id
	GROUP BY L.Name, L.Number
```

...selects the first row of each set of duplicates (in no particular order):

```
27	David	1
29	David	2
16	Alice	3
```

If we join this subquery against the original table, using the greater-than operator...

```c#
SELECT #t.* FROM
	( SELECT MIN(L.Id) AS MinId, L.Name, L.Number FROM #t AS L
		INNER JOIN #t AS R
		ON L.Name = R.Name AND L.Number = R.Number
		AND L.Id <> R.Id
		GROUP BY L.Name, L.Number ) AS Q
INNER JOIN #t
	ON #t.Id > Q.MinId
	AND #t.Name = Q.Name
	AND #t.Number = Q.Number
```

...then we end up with the duplicate rows...

```
17	Alice	3
18	Alice	3
28	David	1
30	David	2
```

...which we can then delete by changing the `SELECT` to a `DELETE`.

Ta-da!
