---
title: "MSBuild Target Batching (For Each) Simplified"
date: 2010-08-19T09:44:53.000Z
redirect_from: /content/2010/08/msbuild-target-batching-each-simplified
tags: msbuild
---
It's actually quite simple:

```xml
<?xml version="1.0" encoding="utf-8"?>
<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003" ToolsVersion="4.0" DefaultTargets="Default">
   <ItemGroup>
      <ProjectsToPublish Include="AdminConsole\AdminConsole.csproj" />
      <ProjectsToPublish Include="AdminService\AdminService.csproj" />
   </ItemGroup>

   <Target Name="Default">
      <CallTarget Targets="PublishProjectOutput" />
   </Target>

   <Target Name="PublishProjectOutput" Inputs="@(ProjectsToPublish)" Outputs="%(Identity).Dummy">
      <Message Text="@(ProjectsToPublish)" />
   </Target>
</Project>
```

We set up an item group containing the items that we'd like to process. Our "Default" target is just to demonstrate that we don't need to do anything clever with `CallTarget`.

In order to get MSBuild to run the `PublishProjectOutput` target for each item in the item group, the necessary magic is in the `Inputs` and `Outputs` stuff. The trick is that the value in `Outputs` is the item **metadata** of the value in `Inputs`. That is: `%(Identity)` is actually treated as if it was `%(ProjectsToPublish.Identity)`. MSBuild batches where the metadata values match. Since `Identity` is unique, each batch will contain a single item, giving us the "for each" behaviour we're looking for.

In order to get MSBuild to run the target _at all_, we need to specify the output _files_ that will be generated. If we were to specify just `%(Identity)`, MSBuild would decide that the outputs were up-to-date and would skip the target. So we dirty up the output by adding a fake extension to the filename. What this is doesn't particularly matter. `%(Identity).Quack` would work just as well (as long as you don't habitually have files called `Foo.csproj.Quack`, of course).
