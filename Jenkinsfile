pipeline {

    agent { docker { image 'instructure/scala-sbt:1.2' } }

    stages {

        stage('Compile') {
            steps {
                echo "Compiling..."
                sh "sbt compile"
            }
        }

        stage('Test') {
            steps {
                echo "Testing..."
                sh "sbt test"
            }
        }

        stage('Package') {
            steps {
                echo "Packaging..."
                sh "sbt package"
            }
        }

    }
}
