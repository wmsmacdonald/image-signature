'use strict'

const fs = require('fs')

const assert = require('assert')
const _ = require('underscore')
const nj = require('numjs')

const imageSignature = require('../')

describe('imageSignature', function () {
  describe('#generate()', function () {
    this.timeout(20 * 1000)
    it('return signature for small jpg image', function () {
      const img = addAlphas(nj.images.read(__dirname + '/fixtures/istanbul_small.jpg'))
      const imageData = {
        height: img.shape[0],
        width: img.shape[1],
        data: img.flatten().tolist()
      }
      const signature = imageSignature.generate(imageData);
      assert.strictEqual(signature.length, 544)
    })
    it('distance between small and medium should be little', function () {
      const small = addAlphas(nj.images.read(__dirname + '/fixtures/istanbul_small.jpg'))
      const medium = addAlphas(nj.images.read(__dirname + '/fixtures/istanbul_medium.jpg'))
      
      const imageDataSmall = {
        height: small.shape[0],
        width: small.shape[1],
        data: small.flatten().tolist()
      }
      const imageDataMedium = {
        height: medium.shape[0],
        width: medium.shape[1],
        data: medium.flatten().tolist()
      }
      const signatureSmall = imageSignature.generate(imageDataSmall)
      this.timeout(40000)
      const signatureMedium = imageSignature.generate(imageDataMedium)
      const distance = signatureSmall.distance(signatureMedium)
      console.log('distance', distance)
      assert(distance < 0.4)
    })
    it('distance between different images should be large', function () {
      const istanbul = addAlphas(nj.images.read(__dirname + '/fixtures/istanbul_medium.jpg'))
      const spaceship = addAlphas(nj.images.read(__dirname + '/fixtures/spaceship.jpg'))
      
      const imageDataIstanbul = {
        height: istanbul.shape[0],
        width: istanbul.shape[1],
        data: istanbul.flatten().tolist()
      }
      const imageDataSpaceship = {
        height: spaceship.shape[0],
        width: spaceship.shape[1],
        data: spaceship.flatten().tolist()
      }
      this.timeout(40000)
      const signatureIstanbul = imageSignature.generate(imageDataIstanbul)
      const signatureSpaceship = imageSignature.generate(imageDataSpaceship)
      const distance = signatureIstanbul.distance(signatureSpaceship)
      console.log('distance', distance)
      assert(distance > 0.4)
    })
  })
})

function addAlphas(img) {
  const zeros = nj.zeros([img.shape[0], img.shape[1], 1])
  return nj.concatenate(img, zeros)
}
