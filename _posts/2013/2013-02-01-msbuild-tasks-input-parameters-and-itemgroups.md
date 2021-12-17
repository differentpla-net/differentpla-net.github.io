---
title: MSBuild Tasks, Input Parameters and ItemGroups
date: 2013-02-01T16:20:00Z
tags: msbuild
---
While looking through some MSBuild scripts recently at work, I came across some stuff to do with Task output parameters that I thought might be worth blogging about. First, however, I need to talk about Task input parameters.

Let's take the following example MSBuild script:

    <ItemGroup>
		<Service Include="Foo">
			<DependsOn>Bar</DependsOn>
		</Service>
		<Service Include="Bar" />
	</ItemGroup>

	<Target Name="Start">
		<StartService Services="@(Service)" />
	</Target>

We also define a custom task that looks like this:

	public class StartService : Task
	{
		[Required]
		public string Services { get; set; }

		public override bool Execute()
		{
			Console.WriteLine(Services);
			return true;
		}
	}

MSBuild will pass the item group (an array of items with metadata) to the `string` property as a semicolon-delimited string, discarding the metadata.

Note that MSBuild doesn't look at the `string` and call the task for each item. If you want that to happen, you need to look at http://blog.differentpla.net/blog/2010/08/19/msbuild-target-batching-for-each-simplified.

A better way to do this is to declare the property as a string array:

	public class StartService : Task
	{
    	[Required]
		public string[] Service { get; set; }

		public override bool Execute()
		{
			foreach (var s in Service)
				Console.WriteLine(s);
			return true;
		}
	}

By doing this, each item is placed in the array separately.

Note that if you attempt to declare an item as follows:

	<Service Include="Baz;Quuz" />

...you actually get two separate items, "Baz" and "Quux".

If you need the metadata, you should declare the property as `ITaskItem[]`:

	[Required]
	public ITaskItem[] Service { get; set; }

This will allow you to access the metadata attached to the items:

 * `item.ItemSpec` is the "name" or "Identity" value of the item.
 * `item.MetadataNames` contains a list of the metadata items attached to the item. MSBuild attaches a bunch of file-related metadata, such as "FullPath" and "RootDir", even if the item is not a file.

Note that MSBuild 4.0 actually passes `ITaskItem2` instances, but you must still declare the property as `ITaskItem[]`.

Here is where you can see your metadata:

	public override bool Execute()
    {
    	foreach (var s in Service)
        	Console.WriteLine("{0} -> {1}", s.ItemSpec, s.GetMetadata("DependsOn"));
        return true;
    }

As a quick aside, MSBuild won't let you have an empty `Include` attribute, so this is invalid, and causes an error:

	<Service Include="" />

It will trim whitespace, so the following will be treated as "Meh":

	<Service Include="    Meh     " />

If you do the following:

	<Service Include="     p   q   r   " />

...you get an item named "p   q   r".

On the other hand, if you do the following:

	<Service Include="       " />

...the item is discarded with no warning or error.

With that out of the way, we can move on to part 2: "MSBuild Tasks, Output Parameters and Item Groups".
