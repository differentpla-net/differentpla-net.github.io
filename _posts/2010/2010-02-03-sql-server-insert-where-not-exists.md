---
title: "SQL Server: INSERT WHERE NOT EXISTS"
date: 2010-02-03T16:32:08.000Z
---
Because whenever I've been away from SQL Server for more than a couple of weeks, I forget.

<pre>/* Some tables. */
CREATE TABLE X (
	Name VARCHAR(20)
)
GO

CREATE TABLE Y (
	Name VARCHAR(20)
)
GO</pre>

<pre>/* Some sample data. */
INSERT INTO X(Name) VALUES('Alice')

INSERT INTO Y(Name) VALUES('Alice')
INSERT INTO Y(Name) VALUES('Bob')
INSERT INTO Y(Name) VALUES('Bob')
INSERT INTO Y(Name) VALUES('Charles')</pre>

<pre>/* The interesting bit. */
INSERT INTO X(Name)
	SELECT DISTINCT Name FROM Y
	WHERE NOT EXISTS ( SELECT * FROM X WHERE X.Name = Y.Name )</pre>