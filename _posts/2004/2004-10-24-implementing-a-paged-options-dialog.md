---
title: "Implementing a paged Options dialog"
date: 2004-10-24T12:42:00.000Z
redirect_from: /content/2004/10/implementing-a-paged-options-dialog
tags: csharp windows-forms
---
Several popular applications implement their options dialog as a collection of pages. Here's one way to do this in your application.

For example, Internet Explorer uses the familiar tabbed dialog:

[img_assist|nid=51|title=|desc=|link=none|align=left|width=575|height=640]

Visual Studio .NET uses a tree control to organise its options:

[img_assist|nid=53|title=|desc=|link=none|align=left|width=640|height=402]

Mozilla FireFox uses a list control to group its options:

[img_assist|nid=50|title=|desc=|link=none|align=left|width=640|height=524]

We'll look at implementing a paged dialog, using a list control, as FireFox does it.

Our overall aim is that we can use the dialog like this:

```
private void toolsOptionsMenuItem_Click(object sender, System.EventArgs e)
{
    OptionsDialog dlg = new OptionsDialog();
    dlg.Pages.Add(new GeneralOptionsPage());
    dlg.Pages.Add(new DisplayOptionsPage());
    dlg.Pages.Add(new AdvancedOptionsPage());
    DialogResult result = dlg.ShowDialog(this);
}
```

We'll have a dialog (here called `OptionsDialog`) to which we'll add the pages, and it'll be responsible for displaying a list, and switching between them.

First, we need the `OptionsDialog` class. It's a normal dialog, which contains 3 panels. I've hilighted the panels in this picture:

[img_assist|nid=54|title=|desc=|link=none|align=left|width=640|height=462]

The panel across the bottom contains the OK and Cancel buttons; the panel on the left contains the ListView control which will display the icons and text; and the panel on the right is empty -- it'll hold the currently active page.

By setting the Dock, DockBorder and Anchor properties of the various controls on the form, we can ensure that this dialog will resize to accomodate different-sized pages. In order to prevent the user resizing the options dialog, we set `FormBorderStyle` to `FixedDialog`.

We then need to take a look at the code for `OptionsDialog`.

The first item is the `Pages` collection. It's nothing particularly clever:

```
ArrayList _pages = new ArrayList();

public IList Pages
{
    get { return _pages; }
}
```

This probably ought to be made type-safe in future, but it'll do for now.
Each page in this collection will derive from a common base class. Since Windows.Forms doesn't define one, I'll call this `PropertyPage`, similar to that in Win32.

It looks like this:

```
public class PropertyPage : System.Windows.Forms.UserControl
{
    // ...
    public new virtual string Text
    {
        get { return this.GetType().Name; }
    }

    public virtual Image Image
    {
        get { return null; }
    }

    public virtual void OnSetActive()
    {
    }

    public virtual void OnApply()
    {
    }
}
```

More interesting is the code that handles the `Load` event:

```
private void OptionsDialog_Load(object sender,
    System.EventArgs e)
{
```

First we load a default image to be used in the list box if the page itself doesn't provide one:

```
    Bitmap defaultImage = new Bitmap(GetType(),
        "Bitmaps.NullOptionsPage.bmp");
    imageList.Images.Add(defaultImage);
```

Then we iterate over the pages, adding each one to the right-hand panel and to the list box, and working out the size of the largest page, so that we can resize the dialog nicely.

```
    Size maxPageSize = pagePanel.Size;

    foreach (PropertyPage page in _pages)
    {
        pagePanel.Controls.Add(page);

        AddListItemForPage(page);

        if (page.Width > maxPageSize.Width)
            maxPageSize.Width = page.Width;
        if (page.Height > maxPageSize.Height)
            maxPageSize.Height = page.Height;

        page.Dock = DockStyle.Fill;
        page.Visible = false;
    }
```

Note that we set the initial value of `maxPageSize` to the size of the right-hand panel. This allows us to control the minimum size of the options dialog, in case the pages are very small.

Note also that we set the `Dock` property of each page. More importantly, note that we do this _after_ we've looked at its size.

The next few lines resize the Options Dialog so that the largest dialog page fits:

```
    Size newSize = new Size();
    newSize.Width = maxPageSize.Width + (this.Width - pagePanel.Width);
    newSize.Height = maxPageSize.Height + (this.Height - pagePanel.Height);

    this.Size = newSize;
    CenterToParent();
```

Finally we select the first item in the list box:

```
    if (listView.Items.Count != 0)
        listView.Items[0].Selected = true;
}
```

Adding an item to the list box looks like this:

```
void AddListItemForPage(PropertyPage page)
{
    int imageIndex = 0;

    Image image = page.Image;
    if (image != null)
    {
        imageList.Images.Add(image);
        imageIndex = imageList.Images.Count - 1;
    }

    ListViewItem item = new ListViewItem(page.Text, imageIndex);
    item.Tag = page;
    listView.Items.Add(item);
}
```

Then, when the user selects an item in the list, we switch out the right-hand page:

```
PropertyPage _activePage;

private void listView_SelectedIndexChanged(object sender, System.EventArgs e)
{
    if (_activePage != null)
        _activePage.Visible = false;

    if (listView.SelectedItems.Count != 0)
    {
        ListViewItem selectedItem = listView.SelectedItems[0];
        PropertyPage page = (PropertyPage)selectedItem.Tag;
        _activePage = page;
    }

    if (_activePage != null)
    {
        _activePage.Visible = true;
        _activePage.OnSetActive();
    }
}
```

Then all we have to is define a couple of pages to go in the options dialog, and we end up with this:

[img_assist|nid=52|title=|desc=|link=none|align=left|width=640|height=410]

Source code is [on github](https://github.com/rlipscombe/paged-options-dialog).
