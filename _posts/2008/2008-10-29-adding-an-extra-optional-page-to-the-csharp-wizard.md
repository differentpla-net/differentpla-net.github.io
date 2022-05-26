---
title: "Adding an extra (optional) page to the C# Wizard"
date: 2008-10-29T16:53:37.000Z
layout: series
series: winformswizard
tags: csharp wizard windows-forms
---
This seems to be causing a few problems, so I'll quickly walk through it. I'll try to throw some screenshots in later.

In the TestWizard project, choose "Add New Item". In the dialog, choose "Inherited User Control" (it's under Windows Forms). Call it `OptionalPage.cs`. The Inheritance Picker will appear. Choose "InternalWizardPage".

In the forms designer, click on the banner and change the `Title` and `Subtitle` properties to "Optional Page" and "This page is optional".

In the constructor for `TestWizardSheet`, add a line to add the `OptionalPage`:

```c#
    this.Pages.Add(new WelcomePage());
    this.Pages.Add(new MiddlePage());
    this.Pages.Add(new OptionalPage());     // ADD THIS
    this.Pages.Add(new CompletePage());
```

If you run the project now, you'll see that the new page has been inserted in the correct place.

But we want to make it optional, so throw a check box onto `MiddlePage`. Add an event handler for `WizardNext`. Make it look like this:

```c#
    private void MiddlePage_WizardNext(object sender, WizardPageEventArgs e)
    {
        if (checkBox1.Checked)
            e.NewPage = "OptionalPage";
        else
            e.NewPage = "CompletePage";
    }
```

Now, when you run through the wizard, the optional page doesn't appear unless the check box is checked.

But wait. It does appear if you press "Back" on the final page. We'll have to be a bit cleverer. Create a class like this:

```c#
public class TestWizardContext
{
    private bool _showOptionalPage;

    public bool ShowOptionalPage
    {
        get { return _showOptionalPage; }
        set { _showOptionalPage = value; }
    }
}
```

Create an instance in `TestWizardSheet`'s constructor and pass it to each of the pages that care:

```c#
    TestWizardContext context = new TestWizardContext();

    this.Pages.Add(new WelcomePage());
    this.Pages.Add(new MiddlePage(context));
    this.Pages.Add(new OptionalPage());
    this.Pages.Add(new CompletePage(context));
```

You'll need to add a `_context` field to `MiddlePage` and `CompletePage` and initialize it in the constructor. Then, create a handler for the checkbox:

```c#
    private void checkBox1_CheckedChanged(object sender, EventArgs e)
    {
        _context.ShowOptionalPage = checkBox1.Checked;
    }
```

Then, add a handler for `WizardBack` to `CompletePage`:

```c#
    private void CompletePage_WizardBack(object sender, WizardPageEventArgs e)
    {
        if (_context.ShowOptionalPage)
            e.NewPage = "OptionalPage";
        else
            e.NewPage = "MiddlePage";
    }
```

Job done.
