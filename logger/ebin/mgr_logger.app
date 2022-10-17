{application, mgr_logger, [
    {description, "logger"},
    {vsn, "1"},
    {modules, [mgr_logger, api_logger_handler, mgr_logger_sup]},
    {mod, {mgr_logger_sup, []}},
    {applications, []}
]}.
