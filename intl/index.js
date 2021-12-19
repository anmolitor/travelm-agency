const wrap = (fun) =>
  new Proxy(
    {},
    {
      get(_target, arg) {
        return fun(arg);
      },
      has(_target, _arg) {
        return true;
      },
    }
  );

/**
 * An object enabling access to the Intl API via JSON. *
 * */
export default intl_proxy = wrap((json) => {
  try {
    const [api, apiArgs, method, methodArgs] = JSON.parse(json);
    return new Intl[api](...apiArgs)[method](...methodArgs);
  } catch (_e) {
    return undefined;
  }
});
