---
title: "Did you know that .NET already had an IoC container?"
date: 2011-12-20T16:30:49.000Z
x-drupal-nid: 271
---
Talking to [@ptrelford](https://twitter.com/#!/ptrelford) at the [TDD & Refactoring workshop](http://www.codemanship.co.uk/) run by [Jason Gorman](https://twitter.com/#!/jasongorman), Phil mentioned that he'd implemented a really simple IoC container in F#. "But there's a really simple one already built into .NET" says I...

In `System.ComponentModel.Design`, you can find the `IServiceContainer` and `IServiceProvider` interfaces along with the `ServiceContainer` implementation. It's really easy to use:

    IServiceContainer container = new ServiceContainer();
    container.AddService(typeof(IRepository), (c, type) => new NHibernateRepository());
    container.AddService(typeof(IEmailSender), (c, type) => new SmtpEmailSender());
    container.AddService(typeof(LoginController),
        (c, type) =>
            new LoginController(
                 (IRepository)c.GetService(typeof(IRepository)),
                 (IEmailSender)c.GetService(typeof(IEmailSender))));

    LoginController controller = (LoginController)container.GetService(typeof(LoginController));

Done. IoC container in zero lines of code. Take that [Ayende](http://ayende.com/blog/2886/building-an-ioc-container-in-15-lines-of-code) :-)

Of course, this can be cleaned up with some extension methods:

    public static class ServiceContainerExtensions
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
    }

----

### Original Comments

Mark Seemann:

*ServiceContainer isn't a DI Container - it doesn't do composition or lifetime
management, which must be regarded as an absolutely essential minimal feature
set before we can call anything a DI Container.*

*In fact, it's totally useless in relation to DI because the only thing you do
with it is to use it as a [Service
Locator](http://blog.ploeh.dk/2010/02/03/ServiceLocatorIsAnAntiPattern.aspx).*

Roger:

*It's definitely more than just a service locator. For me the service locator
anti-pattern is about handing around the `IServiceProvider` as a kind of bag of
holding. You don't know what's in there, and you don't know what people are
going to pull out of it.*

*In this case, you don't need to pass the `IServiceProvider` around the system,
because you can simply pull out the root service that you require at program
kick-off.  Composition and wiring is then dealt with in the registration steps
(in the factory passed to `AddService`.*

*Yeah, it's not auto-wiring (which isn't that hard to add, really, and I've
already got a 15 line head start on Ayende).*

*And, no, it doesn't do lifetime management, in the sense that you can't
release  services when you're done with them. Nor does it do lifestyle
management -- everything's a singleton (w.r.t. the particular container) -- and
there's no thread (or session) affinity.*

*I contend that it's still a DI Container -- it contains things, and lets you
inject them as dependencies.*

*Not a tremendously useful one, though.*

----

### Auto-wiring

    public static void AddType<TService, TConcrete>(this IServiceContainer container)
    {
       container.AddService(typeof(TService),
          (c, type) =>
          {
             object[] args = ResolveConstructorArguments<TConcrete>(c);
             return Activator.CreateInstance(typeof(TConcrete), args);
          });
    }

    public static void AddType<TConcrete>(this IServiceContainer container)
    {
       container.AddService(typeof(TConcrete),
          (c, type) =>
          {
             object[] args = ResolveConstructorArguments<TConcrete>(c);
             return Activator.CreateInstance(typeof(TConcrete), args);
          });
    }

    private static object[] ResolveConstructorArguments<T>(IServiceContainer container)
    {
       var constructor = typeof (T).GetConstructors().First();

       var arguments = new List<object>();
       var parameters = constructor.GetParameters();
       foreach (var parameter in parameters)
       {
          arguments.Add(container.GetService(parameter.ParameterType));
       }

       return arguments.ToArray();
    }

_Edited to add:_ I should be totally clear here. This is meant as an
educational (even tongue-in-cheek) example. **Don't use it in production**.
Mark's comments are totally valid (even though I'm going to argue
semantics). Use [something -- anything --
else](http://www.hanselman.com/blog/ListOfNETDependencyInjectionContainersIOC.aspx).
Please.
