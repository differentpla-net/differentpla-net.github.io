---
title: "SQL Server: INSERT WHERE NOT EXISTS"
date: 2010-02-03T16:32:08.000Z
tags: sql-server
---
Because whenever I've been away from SQL Server for more than a couple of weeks, I forget.

```sql
/* Some tables. */
CREATE TABLE X (
	Name VARCHAR(20)
)
GO

CREATE TABLE Y (
	Name VARCHAR(20)
)
GO
```

```sql
/* Some sample data. */
INSERT INTO X(Name) VALUES('Alice')

INSERT INTO Y(Name) VALUES('Alice')
INSERT INTO Y(Name) VALUES('Bob')
INSERT INTO Y(Name) VALUES('Bob')
INSERT INTO Y(Name) VALUES('Charles')
```

```sql
/* The interesting bit. */
INSERT INTO X(Name)
	SELECT DISTINCT Name FROM Y
	WHERE NOT EXISTS ( SELECT * FROM X WHERE X.Name = Y.Name )
```
