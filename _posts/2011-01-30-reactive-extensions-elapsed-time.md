---
title: "Reactive Extensions: Elapsed Time"
date: 2011-01-30T15:05:53.000Z
x-drupal-nid: 267
x-needs-review: 2011-01-30T15:05:53.000Z
---
I’m using SqlBulkCopy to insert a large number of rows into a database table. I used Observable.FromEvent to hook the SqlRowsCopied event. Rx provides the .TimeStamp() method, but I wanted to know the elapsed time, so I did the following:

<pre>var bulkCopy = new SqlBulkCopy(connectionString)
{
    DestinationTableName = tableName,
    NotifyAfter = 5000,
    BatchSize = 5000
};

var rowsCopied =
    Observable.FromEvent<SqlRowsCopiedEventHandler, SqlRowsCopiedEventArgs>(
        h => h.Invoke,
        h => bulkCopy.SqlRowsCopied += h,
        h => bulkCopy.SqlRowsCopied -= h);
rowsCopied
    .WithElapsedTime()
    .Sample(TimeSpan.FromSeconds(1))
    .Subscribe(x =>
         Console.WriteLine("{0:N0} rows copied in {1}", x.Value.EventArgs.RowsCopied, x.Elapsed));

bulkCopy.WriteToServer(dataReader);</pre>

Obviously, I had to write the WithElapsedTime extension. It was really easy:

<pre>static class ElapsedTimeObservable
{
    public static IObservable<ElapsedTime<TSource>>
        WithElapsedTime<TSource>(this IObservable<TSource> source)
    {
        var timer = Stopwatch.StartNew();
        return source.Select(x => new ElapsedTime<TSource>(x, timer.Elapsed));
    }
}

internal class ElapsedTime<TSource>
{
    public TSource Value { get; private set; }
    public TimeSpan Elapsed { get; private set; }

    public ElapsedTime(TSource value, TimeSpan elapsed)
    {
        Value = value;
        Elapsed = elapsed;
    }
}</pre>

...and that’s it.