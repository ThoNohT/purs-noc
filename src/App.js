"use strict";

exports.ignore = function (a) {
  return function () {
    return {};
  };
};