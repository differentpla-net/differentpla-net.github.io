---
title: "Recursively Finding Files"
date: 2004-03-21T09:45:00.000Z
x-drupal-nid: 146
x-needs-review: 2004-03-21T09:45:00.000Z
---
On my list of "things to do in my copious free time" is: "write a [replacement Rio Receiver server](/node/view/15) in C#". So I've started putting together snippets of code that might turn out to be useful as I start learning C#. This one is a recursive file finder. It uses delegates.

<pre>using System;
using System.IO;

namespace FindMusic
{
  /// <summary>
  /// Summary description for Class1.
  /// </summary>
  class Class1
  {
    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    static void Main(string[] args)
    {
      string[] logicalDrives = Environment.GetLogicalDrives();
      foreach (string logicalDrive in logicalDrives)
      {
        Console.WriteLine("logicalDrive = {0}", logicalDrive);
      }

      string currentDirectory = Environment.CurrentDirectory;
      Console.WriteLine("currentDirectory = {0}",
                        currentDirectory);

      FindFiles(currentDirectory, new FileCallback(OnFile),
                new DirectoryCallback(OnDirectory));
    }

    static void OnFile(FileInfo fileInfo)
    {
      Console.WriteLine("{0}", fileInfo.FullName);
    }

    static void OnDirectory(DirectoryInfo directoryInfo)
    {
      Console.WriteLine("{0}", directoryInfo.FullName);
    }

    public delegate void FileCallback(FileInfo fileInfo);
    public delegate void DirectoryCallback(DirectoryInfo directoryInfo);

    static void FindFiles(string directoryPath,
                          FileCallback fileCallback,
                          DirectoryCallback directoryCallback)
    {
      DirectoryInfo dir = new DirectoryInfo(directoryPath);

      FileInfo[] childFiles = dir.GetFiles();
      foreach (FileInfo childFile in childFiles)
      {
        fileCallback(childFile);
      }

      DirectoryInfo[] childDirectories = dir.GetDirectories();
      foreach (DirectoryInfo childDirectory in childDirectories)
      {
        directoryCallback(childDirectory);

        FindFiles(childDirectory.FullName,
                  fileCallback, directoryCallback);
      }
    }
  }
}</pre>
