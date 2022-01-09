---
title: "Using PropertyGrid with a dictionary object"
date: 2005-02-26T16:04:00.000Z
redirect_from: /content/2005/02/using-propertygrid-with-dictionary
---
If you try using IDictionary with the PropertyGrid control, the results aren't spectacular:

[img_assist|nid=31|title=|desc=|link=none|align=left|width=640|height=640]

Here's how to do it properly.

The first thing that you need to figure out is that when you associate an object with the PropertyGrid, it asks for that object's type descriptor, and then asks that about which properties it supports.

To fake out the type descriptor, we'll need some kind of adapter object. It'll need to implement ICustomTypeDescriptor:

<pre>class DictionaryPropertyGridAdapter : ICustomTypeDescriptor
{
    IDictionary _dictionary;

    public DictionaryPropertyGridAdapter(IDictionary d)
    {
        _dictionary = d;
    }</pre>

Three of the ICustomTypeDescriptor methods are never called by the property grid, but we'll stub them out properly anyway:

<pre>    public string GetComponentName()
    {
        return TypeDescriptor.GetComponentName(this, true);
    }

    public EventDescriptor GetDefaultEvent()
    {
        return TypeDescriptor.GetDefaultEvent(this, true);
    }

    public string GetClassName()
    {
        return TypeDescriptor.GetClassName(this, true);
    }</pre>

Then there's a whole slew of methods that are called by PropertyGrid, but we don't need to do anything interesting in them:

<pre>    public EventDescriptorCollection GetEvents(Attribute[] attributes)
    {
        return TypeDescriptor.GetEvents(this, attributes, true);
    }

    EventDescriptorCollection System.ComponentModel.ICustomTypeDescriptor.GetEvents()
    {
        return TypeDescriptor.GetEvents(this, true);
    }

    public TypeConverter GetConverter()
    {
        return TypeDescriptor.GetConverter(this, true);
    }

    public object GetPropertyOwner(PropertyDescriptor pd)
    {
        return _dictionary;
    }

    public AttributeCollection GetAttributes()
    {
        return TypeDescriptor.GetAttributes(this, true);
    }

    public object GetEditor(Type editorBaseType)
    {
        return TypeDescriptor.GetEditor(this, editorBaseType, true);
    }

    public PropertyDescriptor GetDefaultProperty()
    {
        return null;
    }

    PropertyDescriptorCollection
        System.ComponentModel.ICustomTypeDescriptor.GetProperties()
    {
        return ((ICustomTypeDescriptor)this).GetProperties(new Attribute[0]);
    }</pre>

Then the interesting bit. We simply iterate over the IDictionary, creating a property descriptor for each entry:

<pre>    public PropertyDescriptorCollection GetProperties(Attribute[] attributes)
    {
        ArrayList properties = new ArrayList();
        foreach (DictionaryEntry e in _dictionary)
        {
            properties.Add(new DictionaryPropertyDescriptor(_dictionary, e.Key));
        }

        PropertyDescriptor[] props =
            (PropertyDescriptor[])properties.ToArray(typeof(PropertyDescriptor));

        return new PropertyDescriptorCollection(props);
    }</pre>

Of course, now we need to implement the DictionaryPropertyDescriptor class:

<pre>class DictionaryPropertyDescriptor : PropertyDescriptor
{
</pre>

PropertyDescriptor provides 3 constructors. We want the one that takes a string and an array of attributes:

<pre>    IDictionary _dictionary;
    object _key;

    internal DictionaryPropertyDescriptor(IDictionary d, object key)
        : base(key.ToString(), null)
    {
        _dictionary = d;
        _key = key;
    }</pre>

The attributes are used by PropertyGrid to organise the properties into categories, to display help text and so on. We don't bother with any of that at the moment, so we simply pass null.

The first interesting member is the PropertyType property. We just get the object out of the dictionary and ask it:

<pre>    public override Type PropertyType
    {
        get { return _dictionary[_key].GetType(); }
    }</pre>

If you knew that all of your values were strings, for example, you could just return typeof(string).

Then we implement SetValue and GetValue:

<pre>    public override void SetValue(object component, object value)
    {
        _dictionary[_key] = value;
    }

    public override object GetValue(object component)
    {
        return _dictionary[_key];
    }</pre>

The `component` parameter passed to these two methods is whatever value was returned from ICustomTypeDescriptor.GetPropertyOwner. If it weren't for the fact that we need the dictionary object in PropertyType, we could avoid using the _dictionary member, and just grab it using this mechanism.

And that's it for interesting things. The rest of the class looks like this:

<pre>    public override bool IsReadOnly
    {
        get { return false; }
    }

    public override Type ComponentType
    {
        get { return null; }
    }

    public override bool CanResetValue(object component)
    {
        return false;
    }

    public override void ResetValue(object component)
    {
    }

    public override bool ShouldSerializeValue(object component)
    {
        return false;
    }
}</pre>

Then you can just use it like this:

<pre>private void Form1_Load(object sender, System.EventArgs e)
{
    IDictionary d = new Hashtable();
    d["Hello"] = "World";
    d["Meaning"] = 42;
    d["Shade"] = Color.ForestGreen;

    propertyGrid1.SelectedObject = new DictionaryPropertyGridAdapter(d);
}</pre>

And it comes out looking like this:

[img_assist|nid=32|title=|desc=|link=none|align=left|width=640|height=640]
