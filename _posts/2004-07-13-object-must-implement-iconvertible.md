---
title: "Object must implement IConvertible"
date: 2004-07-13T15:29:00.000Z
x-drupal-nid: 73
x-needs-review: 2004-07-13T15:29:00.000Z
---
While writing some ASP.NET, I got this error. I've just spent ten minutes figuring it out, so I thought I'd share.

I have the following code in my ASP.NET page. <tt>userName</tt> and <tt>password</tt> are the text box controls on my login page. This code is in the handler for the login button:

<pre>SqlConnection connection = new SqlConnection(connectionString);
SqlCommand command = connection.CreateCommand();
command.CommandText = "SELECT UserID FROM Users "
    + "WHERE UserName = @UserName "
    + "AND UserPassword = @UserPassword";
SqlParameter userNameParameter = command.Parameters.Add(
    "@UserName", SqlDbType.NVarChar, 50);
SqlParameter passwordParameter = command.Parameters.Add(
    "@UserPassword", SqlDbType.NVarChar, 50);

userNameParameter.Value = userName;
passwordParameter.Value = password;

connection.Open();
string userID = (string)command.ExecuteScalar();
connection.Close();</pre>

It causes the "Object must implement IConvertible" error on the call to `ExecuteScalar`.

The problem: you can't pass TextBox controls as parameter values. The code should look like this:

<pre>userNameParameter.Value = userName.Text;
passwordParameter.Value = password.Text;</pre>

Doh!