---
title: "Using SQL Server Compact Edition for Unit Testing"
date: 2009-03-29T09:02:11.000Z
tags: sql-server-ce
redirect_from: /content/2009/03/using-sql-server-compact-edition-unit-testing
---

Ayende shows how to use SQLite with an in-memory database for unit testing your NHibernate code ([here](http://ayende.com/Blog/archive/2006/10/14/UnitTestingWithNHibernateActiveRecord.aspx)). This is a great idea: your unit tests will run more quickly, and you don't have to worry about tearing the database down when you've finished. On the other hand, it's not an exact match for Microsoft SQL Server (which is probably what your production website will run against).

Instead, I decided to use SQL Server Compact Edition (SqlServerCe), which is available in Visual Studio 2008\. This has the disadvantage that it's not in-memory so it's slower (though not as slow as SQL Server Express or proper SQL Server), but still has the advantage that, because the database is stored in a local file, we can set it up and tear it down quickly and easily.

To do that, you need to set up your NHibernate configuration as follows:

```csharp
var configurationProperties = new Dictionary<string, string>();
configurationProperties.Add("hibernate.connection.provider", "NHibernate.Connection.DriverConnectionProvider");
configurationProperties.Add("hibernate.connection.driver_class", "NHibernate.Driver.SqlServerCeDriver");
configurationProperties.Add("hibernate.connection.connection_string", "Data Source=Temp.sdf");
configurationProperties.Add("hibernate.dialect", "NHibernate.Dialect.MsSqlCeDialect");
```

Note that the connection string names a database file that must exist. We handle this by having the unit test SetUp method create a new one:

```csharp
File.Copy("Template.sdf", "Temp.sdf", true);
```

To make this work, you'll need to create a new SQL Server Compact database file and then add it to your project. Use **Add** / **New Item...** / **Local Database**. Make sure that the database is configured as "Build Action" = "Content", so that it's copied to the output directory (i.e. next to the unit test assembly).

We should also get our code to create the database according to the mappings we've set up:

```csharp
const string assemblyForType = "Projects.Model.NHibernate";
configuration.AddAssembly(assemblyForType);
new SchemaExport(configuration).Execute(false, true, false, false);
```

### Using the SQL Server CE Driver

You'll probably see this exception:

> NHibernate.HibernateException: Could not create the driver from NHibernate.Driver.SqlServerCeDriver.

> NHibernate.HibernateException: The IDbCommand and IDbConnection implementation in the assembly System.Data.SqlServerCe could not be found.
>  Ensure that the assembly System.Data.SqlServerCe is located in the application directory or in the Global Assembly Cache.
>  If the assembly is in the GAC, use <qualifyAssembly/> element in the application configuration file to specify the full name of the assembly.

This exception will be thrown when NHibernate can't find the relevant driver for SQL Server CE. To fix it, ensure that you've added a reference to System.Data.SqlServerCe to your project. You'll also have to mark it as "Copy Local" = "true" in the reference properties.

### SQL Server CE is 32-bit only

You might see the following exception:

> System.DllNotFoundException: Unable to load DLL 'sqlceme35.dll': The specified module could not be found. (Exception from HRESULT: 0x8007007E)

Your program is probably running in 64-bit mode; SQL Server CE is 32-bit only. To fix this, go to your project's properties. On the "Build" tab, change the "Platform Target" setting to "x86".

If you're running your unit tests with NUnit, you might need to use nunit-console-x86.exe instead. If you're using ReSharper's NUnit runner (built into Visual Studio 2008), you'll be OK: Visual Studio is a 32-bit application.
