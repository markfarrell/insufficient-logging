"use strict";

var syslog = require("syslog-client");

var client = syslog.createClient("127.0.0.1");

exports.logImpl = function(facility) {
  return function(severity) {
    return function(msgid) {
      return function(msg) {
        return function(onError, onSuccess) {
          client.log(msg, {
            severity : severity,
            facility : facility,
            msgid : msgid
          }, function(err) {
            if(err) {
              onError(err);
            } else {
              onSuccess();
            }
          });
          return function(cancelError, cancelerError, cancelerSuccess) {
            cancelerError(cancelError);
          };
        };
      };
    };
  };
};
