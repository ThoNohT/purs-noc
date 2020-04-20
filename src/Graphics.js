"use strict";

exports.getTransform_ = function(context) {
    return function() {
        if (context.currentTransform) {
            // See compatibility info on
            // https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/currentTransform.
            return context.currentTransform;
        } else {
            return context.mozCurrentTransform;
        }
    }
};

exports.getFillStyle_ = function(context) {
    return function() {
        return context.fillStyle;
    }
};

exports.getStrokeStyle_ = function(context) {
    return function() {
        return context.strokeStyle;
    }
};

exports.getStrokeWidth_ = function(context) {
    return function() {
        return context.lineWidth;
    }
};