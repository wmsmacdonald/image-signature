'use strict'
    
const fs = require('fs')

const assert = require('assert')
const _ = require('underscore')
const nj = require('numjs')

const imageSignature = require('../')

const image_base_dir =  '/fixtures/always_sunny_sample_frames/'
const original_dir = image_base_dir + 'original/'
const scaled_down_dir = image_base_dir + 'scaled_down/'
const scaled_up_dir = image_base_dir + 'scaled_up/'
const compressed_dir = image_base_dir + 'compressed/'

describe('imageSignature', function () {
  // get signatures of original images
  _.range(1, 191).forEach(i => {
    const original_file = __dirname + original_dir + 'frame' + leftPad(i, 3, '0') + '.jpg'
    const scaled_down_file = __dirname + scaled_down_dir + 'frame' + leftPad(i, 3, '0') + '.jpg'
    const scaled_up_file = __dirname + scaled_up_dir + 'frame' + leftPad(i, 3, '0') + '.jpg'
    const compressed_file = __dirname + compressed_dir + 'frame' + leftPad(i, 3, '0') + '.jpg'

    const original_signature = getSignature(original_file)
    const scaled_down_signature = getSignature(scaled_down_file)
    const scaled_up_signature = getSignature(scaled_up_file)
    const compressed_signature = getSignature(compressed_file)

    const scaled_down_distance = imageSignature.distance(original_signature, scaled_down_signature)
    const scaled_up_distance = imageSignature.distance(original_signature, scaled_up_signature)
    const compressed_distance = imageSignature.distance(original_signature, compressed_signature)

    console.log('scaled down distance: ', scaled_down_distance)
    console.log('scaled up distance: ', scaled_up_distance)
    console.log('compressed distance: ', compressed_distance)
  })
	    
})

function getSignature(filename) {

    const img = addAlphas(nj.images.read(filename))
    const imageData = {
      height: img.shape[0],
      width: img.shape[1],
      data: img.flatten().tolist()
    }
    return imageSignature.generate(imageData)
}



function addAlphas(img) {
  const zeros = nj.zeros([img.shape[0], img.shape[1], 1])
  return nj.concatenate(img, zeros)
}

function leftPad(s, width, char) {
    return (s.length >= width) ? s : (new Array(width).join(char) + s).slice(-width);
}
