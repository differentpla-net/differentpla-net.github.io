---
title: "Implementing a Wizard in C#, Part 2"
date: 2005-02-26T12:50:00.000Z
x-drupal-nid: 17
x-needs-review: 2005-02-26T12:50:00.000Z
redirect_from: /content/2005/02/implementing-wizard-c-part-2
layout: series
series: winformswizard
tags: csharp wizard windows-forms
---
In this installment (see [here]({% post_url 2005/2005-02-26-implementing-a-wizard-in-c %}) for the previous installment), we'll be fixing a few things and making the whole thing prettier.

**Update: Source code now lives at [CodePlex](http://winformswizard.codeplex.com)**; please post comments, issues, etc., there instead.

## Implementing the Cancel Button

Actually implementing the cancel button is easy:

<pre>private void cancelButton_Click(object sender, System.EventArgs e)
{
    this.Close();
}</pre>

But that's not all -- we need to check with the active page to see if it wants to close. This is as simple as handling the **Closing** event:

<pre>private void WizardSheet_Closing(object sender, System.ComponentModel.CancelEventArgs e)
{
    if (!cancelButton.Enabled)
        e.Cancel = true;
    else if (!finishButton.Enabled)
        OnQueryCancel(e);
}

protected virtual void OnQueryCancel(CancelEventArgs e)
{
    _activePage.OnQueryCancel(e);
}</pre>

We could just call **OnQueryCancel** from the **cancelButton_Click** handler, except that this doesn't handle the user clicking on the close box. So we have to handle the **Closing** event. However, this is fired whenever the form is closing, so we need to be a bit smarter.

This code checks to see if the cancel button is disabled. If it is, we don't need to tell the page -- because the user could only be closing the wizard with the close box. It could be the Finish button, in which case we don't need to tell the page either.

If we decide to tell the page, we call **OnQueryCancel**, which is implemented pretty much as you'd expect:

<pre>[Category("Wizard")]
public event CancelEventHandler QueryCancel;

public virtual void OnQueryCancel(CancelEventArgs e)
{
    if (QueryCancel != null)
        QueryCancel(this, e);
}</pre>

## Implementing the Banner

First, making it prettier. We'll implement a banner for the middle page.

In the Wizard.UI project, create a new user control. The class is called **WizardBanner**. Its **BackColor** property is set to "Window", and it has two label controls and an etched line on it:

[img_assist|nid=18|width=640|height=133]

The Anchor and Dock properties for the labels and line are set appropriately.

## Fixing the Etched Line

One thing we need to fix is that the etched line isn't drawn in the correct place. This requires a simple change to the **EtchedLine.OnPaint** method, and a new property:

<pre>protected override void OnPaint(PaintEventArgs e)
{
    base.OnPaint(e);

    Brush lightBrush = new SolidBrush(_lightColor);
    Brush darkBrush = new SolidBrush(_darkColor);
    Pen lightPen = new Pen(lightBrush, 1);
    Pen darkPen = new Pen(darkBrush, 1);

    if (this.Edge == EtchEdge.Top)
    {
        e.Graphics.DrawLine(darkPen, 0, 0, this.Width, 0);
        e.Graphics.DrawLine(lightPen, 0, 1, this.Width, 1);
    }
    else if (this.Edge == EtchEdge.Bottom)
    {
        e.Graphics.DrawLine(darkPen, 0, this.Height - 2,
            this.Width, this.Height - 2);
        e.Graphics.DrawLine(lightPen, 0, this.Height - 1,
            this.Width, this.Height - 1);
    }
}

EtchEdge _edge = EtchEdge.Top;

[Category("Appearance")]
public EtchEdge Edge
{
    get
    {
        return _edge;
    }

    set
    {
        _edge = value;
        Refresh();
    }
}</pre>

**EtchEdge** is a public enum at namespace level, with two members: **Top** and **Bottom**.

## Banner Properties

We need to add a couple of properties to the banner class before it's useful. These control the text displayed:

<pre>[Category("Appearance")]
public string Title
{
    get { return titleLabel.Text; }
    set { titleLabel.Text = value; }
}

[Category("Appearance")]
public string Subtitle
{
    get { return subtitleLabel.Text; }
    set { subtitleLabel.Text = value; }
}</pre>

Then we can drop it onto the Internal wizard page, dock it, give it a name and set it to public:

[img_assist|nid=19|title=|desc=|link=none|align=left|width=640|height=219]

Making it public means that we can change the text in **MiddlePage**, which is derived from **InternalWizardPage**:

[img_assist|nid=20|title=|desc=|link=none|align=left|width=640|height=309]

Next we need to implement the sidebar for the outer pages. This is done similarly.

## The Sidebar

We add a new user control to the **Wizard.UI** project, called **WizardSidebar**. It does nothing particularly interesting. Then we drop it onto **ExternalWizardPage**, and set its properties appropriately. The background colour of the **ExternalWizardPage** form is set to "Window". If we drop some controls onto the WelcomePage form and set the sidebar graphic, we'll end up with something that looks like this:

[img_assist|nid=21|title=|desc=|link=none|align=left|width=640|height=523]

And that's much prettier.

And that's about it. Source code is attached.

**Update:** Source code now lives [at CodePlex](http://winformswizard.codeplex.com/).
