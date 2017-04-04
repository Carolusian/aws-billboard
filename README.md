# aws-billboard

**aws-billboard** is a dashboard to monitor AWS billing history. The purpose of this project is to help you better plan your AWS purchases.

## Deployment Steps

* Install `unzip`: `sudo apt-get install unzip -y`;
* Install `docker` and `docker-compose` (follow steps here: https://docs.docker.com/engine/installation/linux/ubuntu/);
* Install `aws-cli`: `pip install awscli`;
* Turn on AWS Cost and Usage reports: http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/billing-getting-started.html#step-2;
* Wait a few days for the reports to be generated in your S3 bucket;
* Create an IAM and grant access to your bucket: https://aws.amazon.com/cn/blogs/security/writing-iam-policies-how-to-grant-access-to-an-amazon-s3-bucket/;
* Properly configure your awscli with `aws configure` using your IAM credential;
* Clone this repo: `git clone https://github.com/Carolusian/aws-billboard.git`
* Then, download your AWS billing reports: `cd aws-billing && bash get_billing_reports.sh` 
* Launch your server with: `docker-compose up`
* Open your browser: `http://127.0.0.1:3838`
