---
title: "Using Castle Windsor for ASP.NET MVC Controller classes"
date: 2009-03-29T09:41:41.000Z
---
If you want to use Castle Windsor's MicroKernel (or another IoC container) for ASP.NET MVC controllers, it's fairly easy. The ASP.NET MVC team allowed you to replace the factory used to create controller objects.

In Global.asax.cs, add the following:

<pre>protected void Application_Start()
{
    IWindsorContainer container = new WindsorContainer(new XmlInterpreter());
    Application["WindsorContainer"] = container;

    AddControllersToContainer(container);

    ControllerBuilder.Current.SetControllerFactory(typeof(WindsorControllerFactory));
    RegisterRoutes(RouteTable.Routes);
}

protected void Application_End()
{
    IWindsorContainer container = (IWindsorContainer)Application["WindsorContainer"];
    container.Dispose();

    Application["WindsorContainer"] = null;
}

public static void AddControllersToContainer(IWindsorContainer container)
{
    Assembly.GetExecutingAssembly().GetExportedTypes()
        .Where(type => typeof(IController).IsAssignableFrom(type))
        .ForEach(type => container.AddComponentWithLifestyle(type.Name.ToLower(), type, LifestyleType.Transient));
}

public IWindsorContainer Container
{
    get { return (IWindsorContainer)Application["WindsorContainer"]; }
}</pre>

You'll need [an implementation of ForEach that works on IEnumerable]({% post_url 2009/2009-03-29-linqs-foreach-doesnt-work-on-ienumerable-t %}).

You'll need a controller factory:

<pre>public class WindsorControllerFactory : IControllerFactory
{
    private static IWindsorContainer _container;

    public static IWindsorContainer Container
    {
        get
        {
            // Allow unit tests to override the container.
            return _container ?? (IWindsorContainer)HttpContext.Current.Application["WindsorContainer"];
        }
        set { _container = value; }
    }

    public IController CreateController(RequestContext requestContext, string controllerName)
    {
        return (IController)Container.Resolve(controllerName.ToLower() + "controller");
    }

    public void ReleaseController(IController controller)
    {
        Container.Release(controller);
    }
}</pre>

See [ASP.NET MVC Framework: Create your own IControllerFactory and use Spring.NET](http://weblogs.asp.net/fredriknormen/archive/2007/11/17/asp-net-mvc-framework-create-your-own-icontrollerfactory-and-use-spring-net.aspx)
