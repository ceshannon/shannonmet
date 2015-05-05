{application,shannonmet,
             [{description,"a comet server"},
              {vsn,"1"},
              {registered,[shannonmet_sup]},
              {applications,[kernel,stdlib,cowboy]},
              {mod,{shannonmet_app,[]}},
              {env,[{redis_pool,[{size,10},
                                 {hostname,"127.0.0.1"},
                                 {port,6379},
                                 {password,"123321"}]}]},
              {modules,[delivery_handler,eventsource_handler,redis_worker,
                        shannonmet,shannonmet_app,shannonmet_config,
                        shannonmet_data_protocol,shannonmet_handler,
                        shannonmet_session,shannonmet_session_sup,
                        shannonmet_sup,uuids]}]}.