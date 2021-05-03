const ECS_CLUSTER_NAME = process.env.ECS_CLUSTER_NAME;
const ECS_TASK_DEFINITION = process.env.ECS_TASK_DEFINITION;
const ECS_TASK_VPC_SUBNET_1 = process.env.ECS_TASK_VPC_SUBNET_1;
const ECS_TASK_VPC_SUBNET_2 = process.env.ECS_TASK_VPC_SUBNET_2;
const S3_BUCKET = process.env.S3_BUCKET;

const AWS = require('aws-sdk');
const ECS = new AWS.ECS();

exports.lambda_handler = function (event, context) {
  
  
  runThumbnailGenerateTask(event, S3_BUCKET);
  console.log('Request received:\n', JSON.stringify(event));
};



const runThumbnailGenerateTask = (event, S3_BUCKET) => {
  console.log(event.Records[0].s3.object.key)

  // run an ECS Fargate task
  const params = {
    cluster: `${ECS_CLUSTER_NAME}`,
    launchType: 'FARGATE',
    taskDefinition: `${ECS_TASK_DEFINITION}`,
    count: 1,
    platformVersion:'LATEST',
    networkConfiguration: {
      awsvpcConfiguration: {
          subnets: [
              `${ECS_TASK_VPC_SUBNET_1}`,
              `${ECS_TASK_VPC_SUBNET_2}`
          ],
          assignPublicIp: 'ENABLED'
      }
    },
    overrides: {
      containerOverrides: [
        {
          name: 'geomx-container',
          environment: [
            {
              name: 'S3_BUCKET',
              value: `${S3_BUCKET}`
            },
            {
              name: 'File',
              value: `${event.Records[0].s3.object.key}`
            }
          ]
        }
      ]
    }
  };
  console.log("run ECS params: " + JSON.stringify(params));

  ECS.runTask(params, function (err, data) {
      if (err) {
        console.log(`Error processing ECS Task ${params.taskDefinition}: ${err}`);
      } else {
        console.log(`ECS Task ${params.taskDefinition} started: ${JSON.stringify(data.tasks)}`);
      }
      return;
  });
  // ecsApi.runECSTask(params);
};