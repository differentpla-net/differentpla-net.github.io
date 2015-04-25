---
title: "Did you know that .NET already had an IoC container?"
date: 2011-12-20T16:30:49.000Z
x-drupal-nid: 271
x-needs-review: 2011-12-20T16:30:49.000Z
---
Talking to [@ptrelford](https://twitter.com/#!/ptrelford) at the [TDD & Refactoring workshop](http://www.codemanship.co.uk/) run by [Jason Gorman](https://twitter.com/#!/jasongorman), Phil mentioned that he'd implemented a really simple IoC container in F#. "But there's a really simple one already built into .NET" says I...

In <tt>System.ComponentModel.Design</tt>, you can find the <tt>IServiceContainer</tt> and <tt>IServiceProvider</tt> interfaces along with the <tt>ServiceContainer</tt> implementation. It's really easy to use:

<pre>IServiceContainer container = new ServiceContainer();
container.AddService(typeof(IRepository), (c, type) => new NHibernateRepository());
container.AddService(typeof(IEmailSender), (c, type) => new SmtpEmailSender());
container.AddService(typeof(LoginController),
    (c, type) =>
        new LoginController(
             (IRepository)c.GetService(typeof(IRepository)),
             (IEmailSender)c.GetService(typeof(IEmailSender))));

LoginController controller = (LoginController)container.GetService(typeof(LoginController));
</pre>

Done. IoC container in zero lines of code. Take that [Ayende](http://ayende.com/blog/2886/building-an-ioc-container-in-15-lines-of-code) :-)

Of course, this can be cleaned up with some extension methods:

<pre>public static class ServiceContainerExtensions
{
   public static void AddService<T>(this IServiceContainer container, Func<IServiceProvider, T> factory)
   {
      container.AddService(typeof(T), (c, type) => factory(c));
   }
}

public static class ServiceProviderExtensions
{
   public static T GetService<T>(this IServiceProvider provider)
   {
      return (T)provider.GetService(typeof(T));
   }
}</pre>

_Edited to add:_ I should be totally clear here. This is meant as an educational (even tongue-in-cheek) example. **Don't use it in production**. Mark's comments below are totally valid (even though I'm going to argue semantics). Use [something -- anything -- else](http://www.hanselman.com/blog/ListOfNETDependencyInjectionContainersIOC.aspx). Please.