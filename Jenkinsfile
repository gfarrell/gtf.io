pipeline {
    agent {
        docker { image 'debian:buster-slim' }
    }
    stages {
        stage('deps') {
            environment {
                HUGO_URL = 'https://github.com/gohugoio/hugo/releases/download/v0.55.6/hugo_extended_0.55.6_Linux-64bit.deb'
                HUGO_CHECKSUM = '666b726236327f72a126093c268a49b44488db7b18498bfb5156eaec1421d7f7'
            }
            steps {
                sh 'wget -O "hugo.deb" "$HUGO_URL"'
                sh 'sudo dpkg -i hugo.deb'
                sh 'rm -f hugo.deb'
            }
        }
        stage('build') {
            steps {
                sh 'make build'
            }
        }
        stage('deploy') {
            steps {
                sh 'make deploy'
            }
        }
    }
    post {
        success {
            archiveArtifacts artifacts: 'public', fingerprint: true
        }
        failure {
            mail to: 'gideon@gtf.io',
                 subject: "Build Failure: ${currentBuild.fullDisplayName}",
                 body: "Build failed at ${env.BUILD_URL}"
        }
    }
}
