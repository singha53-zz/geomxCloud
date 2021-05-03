
var aws = require("aws-sdk");
var ses = new aws.SES({ region: "us-east-1" });
const s3 = new aws.S3()

exports.lambda_handler = async function (event) {
    console.log('Request received:\n', JSON.stringify(event));

    
const myBucket = 'geomx-download-bucket'
const myKey = `${event.Records[0].s3.object.key}`
const signedUrlExpireSeconds = 60 * 5

const url = s3.getSignedUrl('getObject', {
    Bucket: myBucket,
    Key: myKey,
    Expires: signedUrlExpireSeconds
})
    
  var params = {
    Destination: {
      ToAddresses: [`${myKey.split('-')[0]}@gmail.com`],
    },
    Message: {
      Body: {
        Html: {
     Charset: "UTF-8", 
     Data: `Please download the R file at the following link and upload to GeoMx Cloud: <a class=\"ulink\" href=\"${url}\" target=\"_blank\">RDS file</a>.`
    },
      },

      Subject: { Data: "GeoMx Cloud - your analyses has completed :)" },
    },
    Source: "asingh.analytics@gmail.com",
  };
 
  return ses.sendEmail(params).promise()
};
